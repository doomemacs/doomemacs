;;; core/autoload/debug.el -*- lexical-binding: t; -*-

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

(defun doom-template-insert (template)
  "TODO"
  (let ((file (expand-file-name (format "templates/%s" template) doom-core-dir)))
    (when (file-exists-p file)
      (insert-file-contents file))))

(defsubst doom--collect-forms-in (file form)
  (when (file-readable-p file)
    (let (forms)
      (with-temp-buffer
        (insert-file-contents file)
        (delay-mode-hooks (emacs-lisp-mode))
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
        (doom-modules (doom-modules)))
    (cl-letf
        (((symbol-function 'sh)
          (lambda (&rest args)
            (cdr (apply #'doom-call-process args)))))
      `((emacs
         (version . ,emacs-version)
         (features ,@system-configuration-features)
         (build . ,(format-time-string "%b %d, %Y" emacs-build-time))
         (buildopts ,system-configuration-options)
         (windowsys . ,(if noninteractive 'batch window-system))
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
    (print! "Doom v%s (Emacs v%s)\nBranch: %s\nCommit: %s\nBuild date: %s"
            doom-version
            emacs-version
            (or (vc-git--symbolic-ref doom-core-dir)
                "n/a")
            (or (vc-git-working-revision doom-core-dir)
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
      (unless (or noninteractive
                  (eq major-mode 'markdown-mode)
                  (not (fboundp 'markdown-mode)))
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
      (if noninteractive
          (print! (buffer-string))
        (switch-to-buffer buffer)
        (kill-new (buffer-string))
        (print! (green "Copied markdown to clipboard"))))))

;;;###autoload
(defun doom/am-i-secure ()
  "Test to see if your root certificates are securely configured in emacs."
  (declare (interactive-only t))
  (interactive)
  (unless (string-match-p "\\_<GNUTLS\\_>" system-configuration-features)
    (warn "gnutls support isn't built into Emacs, there may be problems"))
  (if-let* ((bad-hosts
             (cl-loop for bad
                      in '("https://wrong.host.badssl.com/"
                           "https://self-signed.badssl.com/")
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
      (insert
       (prin1-to-string
        (macroexp-progn
         (append `((setq noninteractive nil
                         init-file-debug t
                         load-path ',load-path
                         package--init-file-ensured t
                         package-user-dir ,package-user-dir
                         package-archives ',package-archives
                         user-emacs-directory ,doom-emacs-dir)
                   (with-eval-after-load 'undo-tree
                     ;; undo-tree throws errors because `buffer-undo-tree' isn't
                     ;; corrrectly initialized
                     (setq-default buffer-undo-tree (make-undo-tree))))
                 (pcase mode
                   (`vanilla-doom+ ; Doom core + modules - private config
                    `((load-file ,(expand-file-name "core.el" doom-core-dir))
                      (doom-initialize)
                      (doom-initialize-core)
                      (add-hook 'window-setup-hook #'doom-display-benchmark-h)
                      (setq doom-modules ',doom-modules)
                      (maphash (lambda (key plist)
                                 (let ((doom--current-module key)
                                       (doom--current-flags (plist-get plist :flags)))
                                   (load! "init" (doom-module-locate-path (car key) (cdr key)) t)))
                               doom-modules)
                      (maphash (lambda (key plist)
                                 (let ((doom--current-module key)
                                       (doom--current-flags (plist-get plist :flags)))
                                   (load! "config" (doom-module-locate-path (car key) (cdr key)) t)))
                               doom-modules)
                      (run-hook-wrapped 'doom-init-modules-hook #'doom-try-run-hook)
                      (doom-run-all-startup-hooks-h)))
                   (`vanilla-doom  ; only Doom core
                    `((load-file ,(expand-file-name "core.el" doom-core-dir))
                      (doom-initialize)
                      (doom-initialize-core)
                      (doom-run-all-startup-hooks-h)))
                   (`vanilla       ; nothing loaded
                    `((package-initialize)))))))
       "\n(unwind-protect (progn\n" contents "\n)\n"
       (format "(delete-file %S))" file)))
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

(fset 'doom--run-vanilla-emacs (lambda! (doom--run-sandbox 'vanilla)))
(fset 'doom--run-vanilla-doom  (lambda! (doom--run-sandbox 'vanilla-doom)))
(fset 'doom--run-vanilla-doom+ (lambda! (doom--run-sandbox 'vanilla-doom+)))
(fset 'doom--run-full-doom     (lambda! (doom--run-sandbox 'doom)))

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
        (doom-template-insert "VANILLA_SANDBOX")
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

;;;###autoload
(defun doom/toggle-debug-mode (&optional arg)
  "Toggle `debug-on-error' and `doom-debug-mode' for verbose logging."
  (interactive (list (or current-prefix-arg 'toggle)))
  (let ((value
         (cond ((eq arg 'toggle) (not doom-debug-mode))
               ((> (prefix-numeric-value arg) 0)))))
    (setq doom-debug-mode value
          debug-on-error value
          garbage-collection-messages value
          use-package-verbose value
          jka-compr-verbose value
          lsp-log-io value
          gcmh-verbose value
          magit-refresh-verbose value)
    (message "Debug mode %s" (if value "on" "off"))))
