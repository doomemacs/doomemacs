;;; core/cli/test.el -*- lexical-binding: t; -*-

(defun doom--emacs-binary ()
  (let ((emacs-binary-path (doom-path invocation-directory invocation-name))
        (runemacs-binary-path (if IS-WINDOWS (doom-path invocation-directory "runemacs.exe"))))
    (if (and runemacs-binary-path (file-exists-p runemacs-binary-path))
        runemacs-binary-path
      emacs-binary-path)))

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
        ;; FIXME Module targets don't work
        (cond ((equal target ":core")
               (appendq! files (nreverse (doom-glob doom-core-dir "test/test-*.el"))))
              ((file-directory-p target)
               (setq target (expand-file-name target))
               (appendq! files (nreverse (doom-glob target "test/test-*.el"))))
              ((file-exists-p target)
               (push target files)))))
    (with-temp-buffer
      (print! (start "Bootstrapping test environment, if necessary..."))
      (if (zerop
           (call-process
            (doom--emacs-binary)
            nil t nil "--batch"
            "--eval" (prin1-to-string
                      `(progn
                         (setq doom-emacs-dir ,doom-emacs-dir
                               doom-local-dir ,(concat doom-local-dir "test/")
                               doom-private-dir ,(concat doom-core-dir "test/"))
                         (require 'core ,(locate-library "core"))
                         (doom-initialize 'force)
                         (doom-initialize-modules)
                         (require 'core-cli)
                         (doom-reload-core-autoloads 'force)
                         (when (doom-packages-install 'auto-accept)
                           (doom-reload-package-autoloads 'force))))))
          (message "%s" (buffer-string))
        (message "%s" (buffer-string))
        (error "Failed to bootstrap unit tests")))
    (dolist (file files)
      (if (doom-file-cookie-p file "if" t)
        (with-temp-buffer
          (unless
              (zerop
               (apply #'call-process
                      (doom--emacs-binary)
                      nil t nil "--batch"
                      (append (list
                               "-L" doom-core-dir
                               "-l" "core"
                               "-l" (concat doom-core-dir "test/helpers.el"))
                              (when (file-in-directory-p file doom-modules-dir)
                                (list "-f" "doom-initialize-core"))
                              (list
                               "-l" file
                               "-f" "buttercup-run"))))
            (setq error t))
          (message "%s" (buffer-string)))
        (print! (info "Ignoring %s" (relpath file)))))
    (if error
        (user-error "A test failed")
      t)))
