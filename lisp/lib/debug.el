;;; lisp/lib/debug.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;
;;; Doom's debug mode

;;;###autoload
(defvar doom-debug-variables
  `(;; Doom variables
    (doom-print-minimum-level . debug)
    (doom-inhibit-log . nil)
    (doom-log-level . 2)

    ;; Emacs variables
    async-debug
    debug-on-error
    gcmh-verbose
    init-file-debug
    jka-compr-verbose
    (message-log-max . 16384)
    (native-comp-async-report-warnings-errors . silent)
    (native-comp-warning-on-missing-source . t)
    url-debug
    use-package-verbose
    (warning-suppress-types . nil))
  "A list of variable to toggle on `doom-debug-mode'.

Each entry can be a variable symbol or a cons cell whose CAR is the variable
symbol and CDR is the value to set it to when `doom-debug-mode' is activated.")

(defvar doom-debug--unbound-vars nil)

(defun doom-debug--watch-vars-h (&rest _)
  (when-let (vars (copy-sequence doom-debug--unbound-vars))
    (setq doom-debug--unbound-vars nil)
    (mapc #'doom-debug--set-var vars)))

(defun doom-debug--set-var (spec)
  (cond ((listp spec)
         (pcase-let ((`(,var . ,val) spec))
           (if (boundp var)
               (set-default
                var (if (not doom-debug-mode)
                        (prog1 (get var 'initial-value)
                          (put var 'initial-value nil))
                      (doom-log "debug:vars: %s = %S" var (default-toplevel-value var))
                      (put var 'initial-value (default-toplevel-value var))
                      val))
             (add-to-list 'doom-debug--unbound-vars spec))))
        ((boundp spec)
         (doom-log "debug:vars: %s = %S" spec doom-debug-mode)
         (set-default-toplevel-value spec doom-debug-mode))
        ((add-to-list 'doom-debug--unbound-vars (cons spec t)))))

;;;###autoload
(define-minor-mode doom-debug-mode
  "Toggle `debug-on-error' and `init-file-debug' for verbose logging."
  :global t
  (let ((enabled doom-debug-mode))
    (doom-log "debug: enabled!")
    (mapc #'doom-debug--set-var doom-debug-variables)
    ;; Watch for changes in `doom-debug-variables', or when packages load (and
    ;; potentially define one of `doom-debug-variables'), in case some of them
    ;; aren't defined when `doom-debug-mode' is first loaded.
    (cond (enabled
           (unless noninteractive
             (message "Debug mode enabled! (Run 'M-x view-echo-area-messages' to open the log buffer)"))
           ;; Produce more helpful (and visible) error messages from errors
           ;; emitted from hooks (particularly mode hooks), that usually go
           ;; unnoticed otherwise.
           (advice-add #'run-hooks :override #'doom-run-hooks)
           ;; Add time stamps to lines in *Messages*
           (advice-add #'message :before #'doom--timestamped-message-a)
           ;; The constant debug output from GC is mostly unhelpful. I still
           ;; want it logged to *Messages*, just out of the echo area.
           (advice-add #'gcmh-idle-garbage-collect :around #'doom-debug-shut-up-a)
           (add-variable-watcher 'doom-debug-variables #'doom-debug--watch-vars-h)
           (add-hook 'after-load-functions #'doom-debug--watch-vars-h))
          (t
           (advice-remove #'run-hooks #'doom-run-hooks)
           (advice-remove #'message #'doom--timestamped-message-a)
           (advice-remove #'gcmh-idle-garbage-collect #'doom-debug-shut-up-a)
           (remove-variable-watcher 'doom-debug-variables #'doom-debug--watch-vars-h)
           (remove-hook 'after-load-functions #'doom-debug--watch-vars-h)
           (doom-log "debug: disabled")
           (message "Debug mode disabled!")))))

(defun doom-debug-shut-up-a (fn &rest args)
  "Suppress output from FN, even in debug mode."
  (let (init-file-debug)
    (apply #'doom-shut-up-a fn args)))


;;
;;; Custom debugger

;; HACK: I advise `debug' instead of changing `debugger' to hide the debugger
;;   itself from the backtrace. Doing it manually would require reimplementing
;;   most of `debug', which is a lot of unnecessary work, when I only want to
;;   decorate the original one slightly.
(defadvice! doom-debugger-a (fn &rest args)
  :around #'debug
  ;; Without `doom-debug-mode', be as vanilla as possible.
  (if (not doom-debug-mode)
      (apply fn args)
    ;; Work around Emacs's heuristic (in eval.c) for detecting errors in the
    ;; debugger, which would run this handler again on subsequent calls. Taken
    ;; from `ert--run-test-debugger'.
    (if (and noninteractive (fboundp 'doom-cli-debugger))
        (apply #'doom-cli-debugger args)
      ;; TODO: Write backtraces to file
      ;; TODO: Write backtrace to a buffer in case recursive error interupts the
      ;;   debugger (happens more often than it should).
      (apply fn args))))

(autoload 'backtrace-get-frames "backtrace")
;;;###autoload
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


;;
;;; Time-stamped *Message* logs

(defun doom--timestamped-message-a (format-string &rest _args)
  "Advice to run before `message' that prepends a timestamp to each message.

Activate this advice with:
(advice-add 'message :before 'doom--timestamped-message-a)"
  (when (and (stringp format-string)
             message-log-max  ; if nil, logging is disabled
             (not (equal format-string "%s%s"))
             (not (equal format-string "\n")))
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
  (setq after-init-time (current-time)
        doom-init-time (current-time))
  (let ((inhibit-startup-hooks nil))
    (doom-run-hooks 'after-init-hook
                    'delayed-warnings-hook
                    'emacs-startup-hook
                    'tty-setup-hook
                    'window-setup-hook
                    'doom-after-init-hook)))


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
  (doom-require 'doom-lib 'profiles)
  (doom-require 'doom-lib 'modules)
  (doom-require 'doom-lib 'packages)
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
                      (if (file-symlink-p file)
                          (concat " -> " (abbrev-path (file-truename file)))
                        ""))))
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
                             (format "EMACSDIR=%s" (symlink-path doom-emacs-dir))
                             (format "EMACS=%s" (expand-file-name invocation-name invocation-directory)))))
        (doom . ,(list doom-version
                       (format "PROFILE=%s" (doom-profile->id (doom-profile-key doom-profile t)))
                       (if (file-exists-p! ".git" doom-emacs-dir)
                           (sh "git" "log" "-1" "--format=%D %h %ci")
                         "[no repo]")
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
                        (if (doom-files-in doom-user-dir :type 'files :match "\\.elc$")
                            'compiled-user-config)
                        (if (doom-files-in doom-core-dir :type 'files :match "\\.elc$")
                            'compiled-core)
                        (if (doom-files-in doom-module-load-path :type 'files :match "\\.elc$")
                            'compiled-modules)))))
        (custom
         ,@(when (and (stringp custom-file)
                      (file-exists-p custom-file))
             (cl-loop for (type var _) in (get 'user 'theme-settings)
                      if (eq type 'theme-value)
                      collect var)))
        (modules
         ,@(or (cl-loop with lastcat = nil
                        for (cat . mod) in (seq-filter #'cdr (doom-module-list))
                        if (or (not lastcat)
                               (not (eq lastcat cat)))
                        do (setq lastcat cat)
                        and collect lastcat
                        collect
                        (let* ((flags (doom-module-get (cons lastcat mod) :flags))
                               (path  (doom-module-get (cons lastcat mod) :path))
                               (module
                                (append
                                 (cond ((null path)
                                        (list '&nopath))
                                       ((not (file-in-directory-p path doom-modules-dir))
                                        (list '&user)))
                                 (if flags
                                     `(,mod ,@flags)
                                   (list mod)))))
                          (if (= (length module) 1)
                              (car module)
                            module)))
               '("n/a")))
        (packages
         ,@(condition-case e
               (mapcar
                #'cdr (doom--collect-forms-in
                       (doom-path doom-user-dir doom-module-packages-file)
                       "package!"))
             (error (format "<%S>" e))))
        (unpin
         ,@(condition-case e
               (mapcan #'identity
                       (mapcar
                        #'cdr (doom--collect-forms-in
                               (doom-path doom-user-dir doom-module-packages-file)
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
                            "git" "-C" (expand-file-name doom-emacs-dir)
                            "log" "-1" "--format=%D %h %ci"))
                      "n/a"))
          ;; NOTE This is a placeholder. Our modules will be moved to its own
          ;;   repo eventually, and Doom core will later be capable of managing
          ;;   them like package sources.
          (format "%-13s v%-15s %s"
                  "Doom modules"
                  doom-modules-version
                  (or (cdr (doom-call-process
                            "git" "-C" (expand-file-name doom-modules-dir)
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

(provide 'doom-lib '(debug))
;;; debug.el ends here
