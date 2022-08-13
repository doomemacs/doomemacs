;;; lisp/lib/debug.el -*- lexical-binding: t; -*-

;;
;;; Doom's debug mode

;;;###autoload
(defvar doom-debug-variables
  '(async-debug
    debug-on-error
    (debugger . doom-debugger)
    (doom-print-level . debug)
    garbage-collection-messages
    gcmh-verbose
    init-file-debug
    jka-compr-verbose
    (message-log-max . 16384)
    (warning-suppress-types . nil)
    url-debug
    use-package-verbose)
  "A list of variable to toggle on `doom-debug-mode'.

Each entry can be a variable symbol or a cons cell whose CAR is the variable
symbol and CDR is the value to set it to when `doom-debug-mode' is activated.")

(defvar doom-debug--undefined-vars nil)

(defun doom-debug--watch-vars-h (&rest _)
  (when-let (bound-vars (cl-delete-if-not #'boundp doom-debug--undefined-vars))
    (doom-log "New variables available: %s" bound-vars)
    (let ((message-log-max nil))
      (doom-debug-mode -1)
      (doom-debug-mode +1))))

;;;###autoload
(define-minor-mode doom-debug-mode
  "Toggle `debug-on-error' and `init-file-debug' for verbose logging."
  :init-value init-file-debug
  :global t
  (let ((enabled doom-debug-mode))
    (setq doom-debug--undefined-vars nil)
    (dolist (var doom-debug-variables)
      (cond ((listp var)
             (pcase-let ((`(,var . ,val) var))
               (if (boundp var)
                   (set-default
                    var (if (not enabled)
                            (prog1 (get var 'initial-value)
                              (put var 'initial-value nil))
                          (put var 'initial-value (symbol-value var))
                          val))
                 (add-to-list 'doom-debug--undefined-vars var))))
            ((if (boundp var)
                 (set-default var enabled)
               (add-to-list 'doom-debug--undefined-vars var)))))
    (when (called-interactively-p 'any)
      (when (fboundp 'explain-pause-mode)
        (explain-pause-mode (if enabled +1 -1))))
    ;; Watch for changes in `doom-debug-variables', or when packages load (and
    ;; potentially define one of `doom-debug-variables'), in case some of them
    ;; aren't defined when `doom-debug-mode' is first loaded.
    (cond (enabled
           (message "Debug mode enabled! (Run 'M-x view-echo-area-messages' to open the log buffer)")
           ;; Produce more helpful (and visible) error messages from errors
           ;; emitted from hooks (particularly mode hooks), that usually go
           ;; unnoticed otherwise.
           (advice-add #'run-hooks :override #'doom-run-hooks)
           ;; Add time stamps to lines in *Messages*
           (advice-add #'message :before #'doom--timestamped-message-a)
           (add-variable-watcher 'doom-debug-variables #'doom-debug--watch-vars-h)
           (add-hook 'after-load-functions #'doom-debug--watch-vars-h))
          (t
           (advice-remove #'run-hooks #'doom-run-hooks)
           (advice-remove #'message #'doom--timestamped-message-a)
           (remove-variable-watcher 'doom-debug-variables #'doom-debug--watch-vars-h)
           (remove-hook 'after-load-functions #'doom-debug--watch-vars-h)
           (message "Debug mode disabled!")))))


;;
;;; Custom debuggers

(autoload 'backtrace-get-frames "backtrace")

(defun doom-backtrace ()
  "Return a stack trace as a list of `backtrace-frame' objects."
  ;; (let* ((n 0)
  ;;        (frame (backtrace-frame n))
  ;;        (frame-list nil)
  ;;        (in-program-stack nil))
  ;;   (while frame
  ;;     (when in-program-stack
  ;;       (push (cdr frame) frame-list))
  ;;     (when (eq (elt frame 1) debugger)
  ;;       (setq in-program-stack t))
  ;;     ;; (when (and (eq (elt frame 1) 'doom-cli-execute)
  ;;     ;;            (eq (elt frame 2) :doom))
  ;;     ;;   (setq in-program-stack nil))
  ;;     (setq n (1+ n)
  ;;           frame (backtrace-frame n)))
  ;;   (nreverse frame-list))
  (cdr (backtrace-get-frames debugger)))

(defun doom-backtrace-write-to-file (backtrace file)
  "Write BACKTRACE to FILE with appropriate boilerplate."
  (make-directory (file-name-directory file) t)
  (let ((doom-print-indent 0))
    (with-temp-file file
      (insert ";; -*- lisp-interaction -*-\n")
      (insert ";; vim: set ft=lisp:\n")
      (insert (format ";; command=%S\n" command-line-args))
      (insert (format ";; date=%S\n\n" (format-time-string "%Y-%m-%d %H-%M-%S" before-init-time)))
      (insert ";;;; ENVIRONMENT\n" (with-output-to-string (doom/version)) "\n")
      (let ((standard-output (current-buffer))
            (print-quoted t)
            (print-escape-newlines t)
            (print-escape-control-characters t)
            (print-symbols-bare t)
            (print-level nil)
            (print-circle nil)
            (n -1))
        (mapc (lambda (frame)
                (princ (format ";;;; %d\n" (cl-incf n)))
                (pp (list (cons (backtrace-frame-fun frame)
                                (backtrace-frame-args frame))
                          (backtrace-frame-locals frame)))
                (terpri))
              backtrace))
      file)))

(defun doom-debugger (&rest args)
  "Enter `debugger' in interactive sessions, `doom-cli-debugger' otherwise.

Writes backtraces to file and ensures the backtrace is recorded, so the user can
always access it."
  (let ((backtrace (doom-backtrace)))
    ;; Work around Emacs's heuristic (in eval.c) for detecting errors in the
    ;; debugger, which would run this handler again on subsequent calls. Taken
    ;; from `ert--run-test-debugger'.
    (cl-incf num-nonmacro-input-events)
    ;; TODO Write backtraces to file
    ;; TODO Write backtrace to a buffer in case recursive error interupts the
    ;;   debugger (happens more often than it should).
    (apply #'debug args)))


;;
;;; Time-stamped *Message* logs

(defun doom--timestamped-message-a (format-string &rest args)
  "Advice to run before `message' that prepends a timestamp to each message.

Activate this advice with:
(advice-add 'message :before 'doom--timestamped-message-a)"
  (when (and (stringp format-string)
             message-log-max
             (not (string-equal format-string "%s%s")))
    (with-current-buffer "*Messages*"
      (let ((timestamp (format-time-string "[%F %T] " (current-time)))
            (deactivate-mark nil))
        (with-silent-modifications
          (goto-char (point-max))
          (if (not (bolp))
              (newline))
          (insert timestamp))))
    (let ((window (get-buffer-window "*Messages*")))
      (when (and window (not (equal (selected-window) window)))
        (with-current-buffer "*Messages*"
          (goto-char (point-max))
          (set-window-point window (point-max)))))))


;;
;;; Hooks

;;;###autoload
(defun doom-run-all-startup-hooks-h ()
  "Run all startup Emacs hooks. Meant to be executed after starting Emacs with
-q or -Q, for example:

  emacs -Q -l init.el -f doom-run-all-startup-hooks-h"
  (setq after-init-time (current-time))
  (let ((inhibit-startup-hooks nil))
    (doom-run-hooks 'after-init-hook
                    'delayed-warnings-hook
                    'emacs-startup-hook
                    'tty-setup-hook
                    'window-setup-hook)))


;;
;;; Helpers

(defsubst doom--collect-forms-in (file form)
  (when (file-readable-p file)
    (let (forms)
      (with-temp-buffer
        (insert-file-contents file)
        (with-syntax-table emacs-lisp-mode-syntax-table
          (while (re-search-forward (format "(%s " (regexp-quote form)) nil t)
            (let ((ppss (syntax-ppss)))
              (unless (or (nth 4 ppss)
                          (nth 3 ppss))
                (save-excursion
                  (goto-char (match-beginning 0))
                  (push (sexp-at-point) forms))))))
        (nreverse forms)))))

;;;###autoload
(defun doom-info ()
  "Returns diagnostic information about the current Emacs session in markdown,
ready to be pasted in a bug report on github."
  (require 'vc-git)
  (require 'doom-packages)
  (let ((default-directory doom-emacs-dir))
    (letf! ((defun sh (&rest args) (cdr (apply #'doom-call-process args)))
            (defun cat (file &optional limit)
              (with-temp-buffer
                (insert-file-contents file nil 0 limit)
                (buffer-string)))
            (defun abbrev-path (path)
              (replace-regexp-in-string
               (regexp-opt (list (user-login-name)) 'words) "$USER"
               (abbreviate-file-name path)))
            (defun symlink-path (file)
              (format "%s%s" (abbrev-path file)
                      (if (file-symlink-p file) ""
                        (concat " -> " (abbrev-path (file-truename file)))))))
      `((generated . ,(format-time-string "%b %d, %Y %H:%M:%S"))
        (system . ,(delq
                    nil (list (doom-system-distro-version)
                              (when (executable-find "uname")
                                (sh "uname" "-msr"))
                              (window-system))))
        (emacs . ,(delq
                   nil (list emacs-version
                             (bound-and-true-p emacs-repository-branch)
                             (and (stringp emacs-repository-version)
                                  (substring emacs-repository-version 0 9))
                             (symlink-path doom-emacs-dir))))
        (doom . ,(list doom-version
                       (sh "git" "log" "-1" "--format=%D %h %ci")
                       (symlink-path doom-user-dir)))
        (shell  . ,(abbrev-path shell-file-name))
        (features . ,system-configuration-features)
        (traits
         . ,(mapcar
             #'symbol-name
             (delq
              nil (list (cond (noninteractive 'batch)
                              ((display-graphic-p) 'gui)
                              ('tty))
                        (if (daemonp) 'daemon)
                        (if (and (require 'server)
                                 (server-running-p))
                            'server-running)
                        (if (boundp 'chemacs-version)
                            (intern (format "chemacs-%s" chemacs-version)))
                        (if (file-exists-p doom-env-file)
                            'envvar-file)
                        (if (featurep 'exec-path-from-shell)
                            'exec-path-from-shell)
                        (if (file-symlink-p doom-emacs-dir)
                            'symlinked-emacsdir)
                        (if (file-symlink-p doom-user-dir)
                            'symlinked-doomdir)
                        (if (and (stringp custom-file) (file-exists-p custom-file))
                            'custom-file)
                        (if (doom-files-in `(,@doom-modules-dirs
                                             ,doom-core-dir
                                             ,doom-user-dir)
                                           :type 'files :match "\\.elc$")
                            'byte-compiled-config)))))
        (custom
         ,@(when (and (stringp custom-file)
                      (file-exists-p custom-file))
             (cl-loop for (type var _) in (get 'user 'theme-settings)
                      if (eq type 'theme-value)
                      collect var)))
        (modules
         ,@(or (cl-loop with cat = nil
                        for key being the hash-keys of doom-modules
                        if (or (not cat)
                               (not (eq cat (car key))))
                        do (setq cat (car key))
                        and collect cat
                        collect
                        (let* ((flags (doom-module-get cat (cdr key) :flags))
                               (path  (doom-module-get cat (cdr key) :path))
                               (module
                                (append
                                 (cond ((null path)
                                        (list '&nopath))
                                       ((not (file-in-directory-p path doom-modules-dir))
                                        (list '&user)))
                                 (if flags
                                     `(,(cdr key) ,@flags)
                                   (list (cdr key))))))
                          (if (= (length module) 1)
                              (car module)
                            module)))
               '("n/a")))
        (packages
         ,@(condition-case e
               (mapcar
                #'cdr (doom--collect-forms-in
                       (doom-path doom-user-dir "packages.el")
                       "package!"))
             (error (format "<%S>" e))))
        (unpin
         ,@(condition-case e
               (mapcan #'identity
                       (mapcar
                        #'cdr (doom--collect-forms-in
                               (doom-path doom-user-dir "packages.el")
                               "unpin!")))
             (error (list (format "<%S>" e)))))
        (elpa
         ,@(condition-case e
               (progn
                 (unless (bound-and-true-p package--initialized)
                   (package-initialize))
                 (cl-loop for (name . _) in package-alist
                          collect (format "%s" name)))
             (error (format "<%S>" e))))))))

;;;###autoload
(defun doom-info-string (&optional width nocolor)
  "Return the `doom-info' as a compact string.

FILL-COLUMN determines the column at which lines will be broken."
  (with-temp-buffer
    (let ((doom-print-backend (unless nocolor doom-print-backend))
          (doom-print-indent 0))
      (dolist (spec (cl-remove-if-not #'cdr (doom-info)) (buffer-string))
        ;; FIXME Refactor this horrible cludge, either here or in `format!'
        (insert! ((bold "%-10s ") (symbol-name (car spec)))
                 ("%s\n"
                  (string-trim-left
                   (indent
                    (fill
                     (if (listp (cdr spec))
                         (mapconcat (doom-partial #'format "%s")
                                    (cdr spec)
                                    " ")
                       (cdr spec))
                     (- (or width 80) 11))
                    11))))))))


;;
;;; Commands

;;;###autoload
(defun doom/version ()
  "Display the running version of Doom core, module sources, and Emacs."
  (interactive)
  (print! "%s\n%s\n%s"
          (format "%-13s v%-15s %s"
                  "GNU Emacs"
                  emacs-version
                  emacs-repository-version)
          (format "%-13s v%-15s %s"
                  "Doom core"
                  doom-version
                  (or (cdr (doom-call-process
                            "git" "-C" doom-emacs-dir
                            "log" "-1" "--format=%D %h %ci"))
                      "n/a"))
          ;; NOTE This is a placeholder. Our modules will be moved to its own
          ;;   repo eventually, and Doom core will later be capable of managing
          ;;   them like package sources.
          (format "%-13s v%-15s %s"
                  "Doom modules"
                  doom-modules-version
                  (or (cdr (doom-call-process
                            "git" "-C" doom-modules-dir
                            "log" "-1" "--format=%D %h %ci"))
                      "n/a"))))

;;;###autoload
(defun doom/info ()
  "Collects some debug information about your Emacs session, formats it and
copies it to your clipboard, ready to be pasted into bug reports!"
  (interactive)
  (let ((buffer (get-buffer-create "*doom info*")))
    (with-current-buffer buffer
      (setq buffer-read-only t)
      (with-silent-modifications
        (erase-buffer)
        (insert (doom-info-string 86)))
      (pop-to-buffer buffer)
      (kill-new (buffer-string))
      (when (y-or-n-p "Your doom-info was copied to the clipboard.\n\nOpen pastebin.com?")
        (browse-url "https://pastebin.com")))))


;;
;;; Profiling

(defvar doom--profiler nil)
;;;###autoload
(defun doom/toggle-profiler ()
  "Toggle the Emacs profiler. Run it again to see the profiling report."
  (interactive)
  (if (not doom--profiler)
      (profiler-start 'cpu+mem)
    (profiler-report)
    (profiler-stop))
  (setq doom--profiler (not doom--profiler)))
