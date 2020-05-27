;;; core/autoload/debug.el -*- lexical-binding: t; -*-

;;
;;; Doom's debug mode

;;;###autoload
(defvar doom-debug-variables
  '(doom-debug-p
    init-file-debug
    debug-on-error
    garbage-collection-messages
    use-package-verbose
    jka-compr-verbose
    lsp-log-io
    gcmh-verbose
    magit-refresh-verbose
    url-debug)
  "A list of variable to toggle on `doom-debug-mode'.")

;;;###autoload
(define-minor-mode doom-debug-mode
  "Toggle `debug-on-error' and `doom-debug-p' for verbose logging."
  :init-value doom-debug-p
  :global t
  (let ((value
         (cond ((eq arg 'toggle) (not doom-debug-mode))
               ((> (prefix-numeric-value arg) 0)))))
    (mapc (doom-rpartial #'set value) doom-debug-variables)
    (message "Debug mode %s" (if value "on" "off"))))


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
  (let ((default-directory doom-emacs-dir)
        (doom-modules (doom-module-list)))
    (letf! (defun sh (&rest args) (cdr (apply #'doom-call-process args)))
      `((emacs
         (version . ,emacs-version)
         (features ,@system-configuration-features)
         (build . ,(format-time-string "%b %d, %Y" emacs-build-time))
         (buildopts ,system-configuration-options)
         (windowsys . ,(if doom-interactive-p window-system 'batch))
         (daemonp . ,(cond ((daemonp) 'daemon)
                           ((and (require 'server)
                                 (server-running-p))
                            'server-running))))
        (doom
         (version . ,doom-version)
         (build . ,(sh "git" "log" "-1" "--format=%D %h %ci"))
         (dir . ,(abbreviate-file-name (file-truename doom-private-dir))))
        (system
         (type . ,system-type)
         (config . ,system-configuration)
         (shell . ,shell-file-name)
         (uname . ,(if IS-WINDOWS
                       "n/a"
                     (sh "uname" "-msrv")))
         (path . ,(mapcar #'abbreviate-file-name exec-path)))
        (config
         (envfile
          . ,(cond ((file-exists-p doom-env-file) 'envvar-file)
                   ((featurep 'exec-path-from-shell) 'exec-path-from-shell)))
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
                         (let ((flags (doom-module-get cat (cdr key) :flags)))
                           (if flags
                               `(,(cdr key) ,@flags)
                             (cdr key))))
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
  (require 'vc-git)
  (let ((default-directory doom-core-dir))
    (print! "Doom v%s (%s)\nEmacs v%s\nBranch: %s\nBuild date: %s"
            doom-version
            (or (vc-git-working-revision doom-core-dir)
                "n/a")
            emacs-version
            (or (vc-git--symbolic-ref doom-core-dir)
                "n/a")
            (or (cdr (doom-call-process "git" "log" "-1" "--format=%ci"))
                "n/a"))))

;;;###autoload
(defun doom/info (&optional raw)
  "Collects some debug information about your Emacs session, formats it into
markdown and copies it to your clipboard, ready to be pasted into bug reports!"
  (interactive "P")
  (let ((buffer (get-buffer-create "*doom-info*"))
        (info (doom-info)))
    (with-current-buffer buffer
      (or (not doom-interactive-p)
          (eq major-mode 'markdown-mode)
          (not (fboundp 'markdown-mode))
          (markdown-mode))
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
        (insert "<details>\n\n```\n")
        (dolist (group info)
          (insert! "%-8s%-10s %s\n"
                   ((car group)
                    (caadr group)
                    (cdadr group)))
          (dolist (spec (cddr group))
            (insert! (indent 8 "%-10s %s\n")
                     ((car spec) (cdr spec)))))
        (insert "```\n</details>"))
      (if (not doom-interactive-p)
          (print! (buffer-string))
        (switch-to-buffer buffer)
        (kill-new (buffer-string))
        (print! (green "Copied markdown to clipboard"))))))

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
                      process-environment ',doom--initial-process-environment
                      exec-path ',doom--initial-exec-path
                      init-file-debug t
                      load-path ',load-path
                      package--init-file-ensured t
                      package-user-dir ,package-user-dir
                      package-archives ',package-archives
                      user-emacs-directory ,doom-emacs-dir)
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
