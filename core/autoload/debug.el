;;; core/autoload/debug.el -*- lexical-binding: t; -*-

;;
;;; Doom's debug mode

;;;###autoload
(defvar doom-debug-variables
  '(async-debug
    debug-on-error
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
               (if (boundp var)
                   (set-default
                    var (if (not enabled)
                            (prog1 (get var 'initial-value)
                              (put 'x 'initial-value nil))
                          (put var 'initial-value (symbol-value var))
                          val))
                 (add-to-list 'doom--debug-vars-undefined var))))
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
        (let (emacs-lisp-mode-hook) (emacs-lisp-mode))
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
                       (symlink-path doom-private-dir)))
        (shell  . ,(abbrev-path shell-file-name))
        (features . ,system-configuration-features)
        (traits
         . ,(mapcar
             #'symbol-name
             (delq
              nil (list (cond ((not doom-interactive-p) 'batch)
                              ((display-graphic-p) 'gui)
                              ('tty))
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
                            'symlinked-doomdir)
                        (if (and (stringp custom-file) (file-exists-p custom-file))
                            'custom-file)
                        (if (doom-files-in `(,@doom-modules-dirs
                                             ,doom-core-dir
                                             ,doom-private-dir)
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
         ,@(condition-case e
               (mapcar
                #'cdr (doom--collect-forms-in
                       (doom-path doom-private-dir "packages.el")
                       "package!"))
             (error (format "<%S>" e))))
        (unpin
         ,@(condition-case e
               (mapcan #'identity
                       (mapcar
                        #'cdr (doom--collect-forms-in
                               (doom-path doom-private-dir "packages.el")
                               "unpin!")))
             (error (list (format "<%S>" e)))))
        (elpa
         ,@(condition-case e
               (progn
                 (package-initialize)
                 (cl-loop for (name . _) in package-alist
                          collect (format "%s" name)))
             (error (format "<%S>" e))))))))


;;
;;; Commands

;;;###autoload
(defun doom/version ()
  "Display the current version and ocmit of Doom & Emacs."
  (interactive)
  (let ((default-directory doom-emacs-dir))
    (print! "Doom emacs\tv%-15s %s"
            doom-version
            (or (cdr (doom-call-process "git" "log" "-1" "--format=%D %h %ci"))
                "n/a")))
  (let ((default-directory doom-core-dir))
    (print! "Doom core\tv%-15s %s"
            doom-core-version
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
        (dolist (spec info)
          (when (cdr spec)
            (insert! "%-11s  %s\n"
                     ((car spec)
                      (if (listp (cdr spec))
                          (mapconcat (lambda (x) (format "%s" x))
                                     (cdr spec) " ")
                        (cdr spec)))))))
      (if (not doom-interactive-p)
          (print! (buffer-string))
        (with-current-buffer (pop-to-buffer buffer)
          (setq buffer-read-only t)
          (goto-char (point-min))
          (kill-new (buffer-string))
          (when (y-or-n-p "Your doom-info was copied to the clipboard.\n\nOpen pastebin.com?")
            (browse-url "https://pastebin.com")))))))


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
;;; Reporting bugs

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
