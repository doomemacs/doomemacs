;;; core/cli/test.el -*- lexical-binding: t; -*-

(defcli! test (&rest targets)
  "Run Doom unit tests."
  (let (files error)
    (unless targets
      (setq targets
            (cons doom-core-dir
                  (cl-remove-if-not
                   (lambda (path) (file-in-directory-p path doom-emacs-dir))
                   ;; Omit `doom-private-dir', which is always first
                   (let (doom-modules)
                     (load! "test/init" doom-core-dir)
                     (cdr (doom-module-load-path)))))))
    (while targets
      (let ((target (pop targets)))
        (cond ((equal target ":core")
               (appendq! files (nreverse (doom-glob doom-core-dir "test/test-*.el"))))
              ((file-directory-p target)
               (setq target (expand-file-name target))
               (appendq! files (nreverse (doom-glob target "test/test-*.el"))))
              ((file-exists-p target)
               (push target files)))))
    (require 'restart-emacs)
    (with-temp-buffer
      (setenv "DOOMDIR" (concat doom-core-dir "test/"))
      (setenv "DOOMLOCALDIR" (concat doom-local-dir "test/"))
      (print! (start "Bootstrapping test environment, if necessary..."))
      (if (zerop
           (call-process
            (restart-emacs--get-emacs-binary)
            nil t nil "--batch"
            "-l" (concat doom-core-dir "core.el")
            "--eval" (prin1-to-string
                      `(progn (doom-initialize 'force)
                              (doom-initialize-modules)
                              (require 'core-cli)
                              (unless (package-installed-p 'buttercup)
                                (package-refresh-contents)
                                (package-install 'buttercup))
                              (doom-reload-core-autoloads 'force)
                              (when (doom-packages-install 'auto-accept)
                                (doom-reload-package-autoloads 'force))))))
          (message "%s" (buffer-string))
        (message "%s" (buffer-string))
        (error "Failed to bootstrap unit tests")))
    (dolist (file files)
      (if (doom-file-cookie-p file)
        (with-temp-buffer
          (unless
              (zerop
               (call-process
                (restart-emacs--get-emacs-binary)
                nil t nil "--batch"
                "-l" (concat doom-core-dir "core.el")
                "-l" (concat doom-core-dir "test/helpers.el")
                "--eval" (prin1-to-string `(doom-initialize 'force))
                "-l" "buttercup"
                "-l" file
                "-f" "buttercup-run"))
            (setq error t))
          (message "%s" (buffer-string)))
        (print! (info "Ignoring %s" (relpath file)))))
    (if error
        (error "A test failed")
      t)))
