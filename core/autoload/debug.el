;;; core/autoload/debug.el -*- lexical-binding: t; -*-

;;
;;; Doom's debug mode

;;;###autoload
(defvar doom-debug-variables
  '(debug-on-error
    doom-debug-p
    garbage-collection-messages
    gcmh-verbose
    init-file-debug
    jka-compr-verbose
    url-debug
    use-package-verbose
    (message-log-max . 16384))
  "A list of variable to toggle on `doom-debug-mode'.

Each entry can be a variable symbol or a cons cell whose CAR is the variable
symbol and CDR is the value to set it to when `doom-debug-mode' is activated.")

(defvar doom--debug-vars-old-values nil)
(defvar doom--debug-vars-undefined nil)

(defun doom--watch-debug-vars-h (&rest _)
  (when-let (bound-vars (cl-remove-if-not #'boundp doom--debug-vars-undefined))
    (doom-log "New variables available: %s" bound-vars)
    (let ((message-log-max nil))
      (doom-debug-mode -1)
      (doom-debug-mode +1))))

;;;###autoload
(define-minor-mode doom-debug-mode
  "Toggle `debug-on-error' and `doom-debug-p' for verbose logging."
  :init-value nil
  :global t
  (let ((enabled doom-debug-mode))
    (setq doom--debug-vars-undefined nil)
    (dolist (var doom-debug-variables)
      (cond ((listp var)
             (cl-destructuring-bind (var . val) var
               (if (not (boundp var))
                   (add-to-list 'doom--debug-vars-undefined var)
                 (set-default
                  var (if (not enabled)
                          (alist-get var doom--debug-vars-old-values)
                        (setf (alist-get var doom--debug-vars-old-values)
                              (symbol-value var))
                        val)))))
            ((if (boundp var)
                 (set-default var enabled)
               (add-to-list 'doom--debug-vars-undefined var)))))
    (when (called-interactively-p 'any)
      (when (fboundp 'explain-pause-mode)
        (explain-pause-mode (if enabled +1 -1))))
    ;; Watch for changes in `doom-debug-variables', or when packages load (and
    ;; potentially define one of `doom-debug-variables'), in case some of them
    ;; aren't defined when `doom-debug-mode' is first loaded.
    (cond (enabled
           (add-variable-watcher 'doom-debug-variables #'doom--watch-debug-vars-h)
           (add-hook 'after-load-functions #'doom--watch-debug-vars-h))
          (t
           (remove-variable-watcher 'doom-debug-variables #'doom--watch-debug-vars-h)
           (remove-hook 'after-load-functions #'doom--watch-debug-vars-h)))
    (message "Debug mode %s" (if enabled "on" "off"))))


;;
;;; Hooks

;;;###autoload
(defun doom-run-all-startup-hooks-h ()
  "Run all startup Emacs hooks. Meant to be executed after starting Emacs with
-q or -Q, for example:

  emacs -Q -l init.el -f doom-run-all-startup-hooks-h"
  (run-hook-wrapped 'after-init-hook #'doom-try-run-hook)
  (setq after-init-time (current-time))
  (mapc (doom-rpartial #'run-hook-wrapped #'doom-try-run-hook)
        (list 'delayed-warnings-hook
              'emacs-startup-hook 'tty-setup-hook
              'window-setup-hook)))


;;
;;; Helpers

(defsubst doom--collect-forms-in (file form)
  (when (file-readable-p file)
    (let (forms)
      (with-temp-buffer
        (insert-file-contents file)
        (let (emacs-lisp-mode) (emacs-lisp-mode))
        (while (re-search-forward (format "(%s " (regexp-quote form)) nil t)
          (let ((ppss (syntax-ppss)))
            (unless (or (nth 4 ppss)
                        (nth 3 ppss))
              (save-excursion
                (goto-char (match-beginning 0))
                (push (sexp-at-point) forms)))))
        (nreverse forms)))))

;;;###autoload
(defun doom-info ()
  "Returns diagnostic information about the current Emacs session in markdown,
ready to be pasted in a bug report on github."
  (require 'vc-git)
  (require 'core-packages)
  (let ((default-directory doom-emacs-dir))
    (letf! ((defun sh (&rest args) (cdr (apply #'doom-call-process args)))
            (defun abbrev-path (path)
              (replace-regexp-in-string
               (regexp-quote (user-login-name)) "$USER"
               (abbreviate-file-name path))))
      `((system
         (type   . ,system-type)
         (config . ,system-configuration)
         (shell  . ,(abbrev-path shell-file-name))
         (uname  . ,(if IS-WINDOWS "n/a" (sh "uname" "-msrv")))
         (path   . ,(mapcar #'abbrev-path exec-path)))
        (emacs
         (dir       . ,(abbrev-path (file-truename doom-emacs-dir)))
         (version   . ,emacs-version)
         (build     . ,(format-time-string "%b %d, %Y" emacs-build-time))
         (buildopts . ,system-configuration-options)
         (features  . ,system-configuration-features)
         (traits . ,(delq
                     nil (list (if (not doom-interactive-p) 'batch)
                               (if (daemonp) 'daemon)
                               (if (and (require 'server)
                                        (server-running-p))
                                   'server-running)
                               (if (boundp 'chemacs-profiles-path)
                                   'chemacs)
                               (if (file-exists-p doom-env-file)
                                   'envvar-file)
                               (if (featurep 'exec-path-from-shell)
                                   'exec-path-from-shell)
                               (if (file-symlink-p user-emacs-directory)
                                   'symlinked-emacsdir)
                               (if (file-symlink-p doom-private-dir)
                                   'symlinked-doomdir)))))
        (doom
         (dir     . ,(abbrev-path (file-truename doom-private-dir)))
         (version . ,doom-version)
         (build   . ,(sh "git" "log" "-1" "--format=%D %h %ci"))
         (elc-files
          . ,(length (doom-files-in `(,@doom-modules-dirs
                                      ,doom-core-dir
                                      ,doom-private-dir)
                                    :type 'files :match "\\.elc$")))
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
                                (module (append (cond ((null path)
                                                       (list '&nopath))
                                                      ((file-in-directory-p path doom-private-dir)
                                                       (list '&user)))
                                                (if flags
                                                    `(,(cdr key) ,@flags)
                                                  (list (cdr key))))))
                           (if (= (length module) 1)
                               (car module)
                             module)))
                '("n/a")))
         (packages
          ,@(or (condition-case e
                    (mapcar
                     #'cdr (doom--collect-forms-in
                            (doom-path doom-private-dir "packages.el")
                            "package!"))
                  (error (format "<%S>" e)))
                '("n/a")))
         (unpin
          ,@(or (condition-case e
                    (mapcan #'identity
                            (mapcar
                             #'cdr (doom--collect-forms-in
                                    (doom-path doom-private-dir "packages.el")
                                    "unpin!")))
                  (error (format "<%S>" e)))
                '("n/a")))
         (elpa
          ,@(or (condition-case e
                    (progn
                      (package-initialize)
                      (cl-loop for (name . _) in package-alist
                               collect (format "%s" name)))
                  (error (format "<%S>" e)))
                '("n/a"))))))))


;;
;;; Commands

;;;###autoload
(defun doom/version ()
  "Display the current version of Doom & Emacs, including the current Doom
branch and commit."
  (interactive)
  (let ((default-directory doom-core-dir))
    (print! "Doom v%s (%s)"
            doom-version
            (or (cdr (doom-call-process "git" "log" "-1" "--format=%D %h %ci"))
                "n/a"))))

;;;###autoload
(defun doom/info (&optional raw)
  "Collects some debug information about your Emacs session, formats it and
copies it to your clipboard, ready to be pasted into bug reports!"
  (interactive "P")
  (let ((buffer (get-buffer-create "*doom info*"))
        (info (doom-info)))
    (with-current-buffer buffer
      (erase-buffer)
      (if raw
          (progn
            (save-excursion
              (pp info (current-buffer)))
            (dolist (sym '(modules packages))
              (when (re-search-forward (format "^ *\\((%s\\)" sym) nil t)
                (goto-char (match-beginning 1))
                (cl-destructuring-bind (beg . end)
                    (bounds-of-thing-at-point 'sexp)
                  (let ((sexp (prin1-to-string (sexp-at-point))))
                    (delete-region beg end)
                    (insert sexp))))))
        (insert "```\n")
        (dolist (group info)
          (insert! "%-8s%-10s %s\n"
                   ((upcase (symbol-name (car group)))
                    (caadr group)
                    (cdadr group)))
          (dolist (spec (cddr group))
            (insert! (indent 8 "%-10s %s\n")
                     ((car spec) (cdr spec)))))
        (insert "```\n"))
      (if (not doom-interactive-p)
          (print! (buffer-string))
        (pop-to-buffer buffer)
        (kill-new (buffer-string))
        (print! (green "Copied your doom info to clipboard"))))))

;;;###autoload
(defun doom/am-i-secure ()
  "Test to see if your root certificates are securely configured in emacs.
Some items are not supported by the `nsm.el' module."
  (declare (interactive-only t))
  (interactive)
  (unless (string-match-p "\\_<GNUTLS\\_>" system-configuration-features)
    (warn "gnutls support isn't built into Emacs, there may be problems"))
  (if-let* ((bad-hosts
             (cl-loop for bad
                      in '("https://expired.badssl.com/"
                           "https://wrong.host.badssl.com/"
                           "https://self-signed.badssl.com/"
                           "https://untrusted-root.badssl.com/"
                           ;; "https://revoked.badssl.com/"
                           ;; "https://pinning-test.badssl.com/"
                           "https://sha1-intermediate.badssl.com/"
                           "https://rc4-md5.badssl.com/"
                           "https://rc4.badssl.com/"
                           "https://3des.badssl.com/"
                           "https://null.badssl.com/"
                           "https://sha1-intermediate.badssl.com/"
                           ;; "https://client-cert-missing.badssl.com/"
                           "https://dh480.badssl.com/"
                           "https://dh512.badssl.com/"
                           "https://dh-small-subgroup.badssl.com/"
                           "https://dh-composite.badssl.com/"
                           "https://invalid-expected-sct.badssl.com/"
                           ;; "https://no-sct.badssl.com/"
                           ;; "https://mixed-script.badssl.com/"
                           ;; "https://very.badssl.com/"
                           "https://subdomain.preloaded-hsts.badssl.com/"
                           "https://superfish.badssl.com/"
                           "https://edellroot.badssl.com/"
                           "https://dsdtestprovider.badssl.com/"
                           "https://preact-cli.badssl.com/"
                           "https://webpack-dev-server.badssl.com/"
                           "https://captive-portal.badssl.com/"
                           "https://mitm-software.badssl.com/"
                           "https://sha1-2016.badssl.com/"
                           "https://sha1-2017.badssl.com/")
                      if (condition-case _e
                             (url-retrieve-synchronously bad)
                           (error nil))
                      collect bad)))
      (error "tls seems to be misconfigured (it got %s)."
             bad-hosts)
    (url-retrieve "https://badssl.com"
                  (lambda (status)
                    (if (or (not status) (plist-member status :error))
                        (warn "Something went wrong.\n\n%s" (pp-to-string status))
                      (message "Your trust roots are set up properly.\n\n%s" (pp-to-string status))
                      t)))))


;;
;;; Vanilla sandbox

(defun doom--run-sandbox (&optional mode)
  (interactive)
  (let ((contents (buffer-string))
        (file (make-temp-file "doom-sandbox-")))
    (require 'package)
    (with-temp-file file
      (prin1 `(progn
                (setq noninteractive nil
                      user-init-file ,file
                      process-environment ',doom--initial-process-environment
                      exec-path ',doom--initial-exec-path
                      init-file-debug t
                      doom--initial-load-path load-path
                      load-path ',load-path
                      package--init-file-ensured t
                      package-user-dir ,package-user-dir
                      package-archives ',package-archives
                      user-emacs-directory ,doom-emacs-dir
                      comp-deferred-compilation nil
                      comp-eln-load-path ',(bound-and-true-p comp-eln-load-path)
                      comp-async-env-modifier-form ',(bound-and-true-p comp-async-env-modifier-form)
                      comp-deferred-compilation-black-list ',(bound-and-true-p comp-deferred-compilation-black-list))
                (with-eval-after-load 'undo-tree
                  ;; undo-tree throws errors because `buffer-undo-tree' isn't
                  ;; correctly initialized
                  (setq-default buffer-undo-tree (make-undo-tree)))
                (ignore-errors
                  (delete-directory ,(expand-file-name "auto-save-list" doom-emacs-dir) 'parents)))
             (current-buffer))
      (prin1 `(unwind-protect
                  (defun --run-- () ,(read (concat "(progn\n" contents "\n)")))
                (delete-file ,file))
             (current-buffer))
      (prin1 (pcase mode
               (`vanilla-doom+ ; Doom core + modules - private config
                `(progn
                   (load-file ,(expand-file-name "core.el" doom-core-dir))
                   (setq doom-modules-dirs (list doom-modules-dir))
                   (let ((doom-init-modules-p t))
                     (doom-initialize)
                     (doom-initialize-core-modules))
                   (setq doom-modules ',doom-modules)
                   (maphash (lambda (key plist)
                              (doom-module-put
                               (car key) (cdr key)
                               :path (doom-module-locate-path (car key) (cdr key))))
                            doom-modules)
                   (--run--)
                   (maphash (doom-module-loader doom-module-init-file) doom-modules)
                   (maphash (doom-module-loader doom-module-config-file) doom-modules)
                   (run-hook-wrapped 'doom-init-modules-hook #'doom-try-run-hook)
                   (doom-run-all-startup-hooks-h)))
               (`vanilla-doom  ; only Doom core
                `(progn
                   (load-file ,(expand-file-name "core.el" doom-core-dir))
                   (let ((doom-init-modules-p t))
                     (doom-initialize)
                     (doom-initialize-core-modules))
                   (--run--)
                   (doom-run-all-startup-hooks-h)))
               (`vanilla       ; nothing loaded
                `(progn
                   (package-initialize)
                   (--run--))))
             (current-buffer)))
    (let ((args (if (eq mode 'doom)
                    (list "-l" file)
                  (list "-Q" "-l" file))))
      (require 'restart-emacs)
      (condition-case e
          (cond ((display-graphic-p)
                 (if (memq system-type '(windows-nt ms-dos))
                     (restart-emacs--start-gui-on-windows args)
                   (restart-emacs--start-gui-using-sh args)))
                ((memq system-type '(windows-nt ms-dos))
                 (user-error "Cannot start another Emacs from Windows shell."))
                ((suspend-emacs
                  (format "%s %s -nw; fg"
                          (shell-quote-argument (restart-emacs--get-emacs-binary))
                          (mapconcat #'shell-quote-argument args " ")))))
        (error
         (delete-file file)
         (signal (car e) (cdr e)))))))

(fset 'doom--run-vanilla-emacs (cmd! (doom--run-sandbox 'vanilla)))
(fset 'doom--run-vanilla-doom  (cmd! (doom--run-sandbox 'vanilla-doom)))
(fset 'doom--run-vanilla-doom+ (cmd! (doom--run-sandbox 'vanilla-doom+)))
(fset 'doom--run-full-doom     (cmd! (doom--run-sandbox 'doom)))

(defvar doom-sandbox-emacs-lisp-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'doom--run-vanilla-emacs)
    (define-key map (kbd "C-c C-d") #'doom--run-vanilla-doom)
    (define-key map (kbd "C-c C-p") #'doom--run-vanilla-doom+)
    (define-key map (kbd "C-c C-f") #'doom--run-full-doom)
    (define-key map (kbd "C-c C-k") #'kill-current-buffer)
    map))

(define-derived-mode doom-sandbox-emacs-lisp-mode emacs-lisp-mode "Sandbox Elisp"
  "TODO")

;;;###autoload
(defun doom/sandbox ()
  "Open the Emacs Lisp sandbox.

This is a test bed for running Emacs Lisp in another instance of Emacs with
varying amounts of Doom loaded, including:

  a) vanilla Emacs (nothing loaded),
  b) vanilla Doom (only Doom core),
  c) Doom + modules - your private config or
  c) Doom + modules + your private config (a complete Doom session)

This is done without sacrificing access to installed packages. Use the sandbox
to reproduce bugs and determine if Doom is to blame."
  (interactive)
  (let* ((buffer-name "*doom:sandbox*")
         (exists (get-buffer buffer-name))
         (buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (doom-sandbox-emacs-lisp-mode)
      (setq-local default-directory doom-emacs-dir)
      (unless (buffer-live-p exists)
        (insert-file-contents (doom-glob doom-core-dir "templates/VANILLA_SANDBOX"))
        (let ((contents (substitute-command-keys (buffer-string))))
          (erase-buffer)
          (insert contents "\n")))
      (goto-char (point-max)))
    (pop-to-buffer buf)))


;;
;;; Reporting bugs

;;;###autoload
(defun doom/report-bug ()
  "Open a markdown buffer destinated to populate the New Issue page on Doom
Emacs' issue tracker.

If called when a backtrace buffer is present, it and the output of `doom-info'
will be automatically appended to the result."
  (interactive)
  (browse-url "https://github.com/hlissner/doom-emacs/issues/new/choose"))

;;;###autoload
(defun doom/copy-buffer-contents (buffer-name)
  "Copy the contents of BUFFER-NAME to your clipboard."
  (interactive
   (list (if current-prefix-arg
             (completing-read "Select buffer: " (mapcar #'buffer-name (buffer-list)))
           (buffer-name (current-buffer)))))
  (let ((buffer (get-buffer buffer-name)))
    (unless (buffer-live-p buffer)
      (user-error "Buffer isn't live"))
    (kill-new
     (with-current-buffer buffer
       (substring-no-properties (buffer-string))))
    (message "Contents of %S were copied to the clipboard" buffer-name)))


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
