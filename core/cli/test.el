;;; core/cli/test.el -*- lexical-binding: t; -*-

(defun doom--emacs-binary ()
  (let ((emacs-binary-path (doom-path invocation-directory invocation-name))
        (runemacs-binary-path (if IS-WINDOWS (doom-path invocation-directory "runemacs.exe"))))
    (if (and runemacs-binary-path (file-exists-p runemacs-binary-path))
        runemacs-binary-path
      emacs-binary-path)))


(defcli! test (&rest targets)
  "Run Doom unit tests."
  :bare t
  (doom-initialize 'force 'noerror)
  (require 'ansi-color)
  (let (files read-files)
    (unless targets
      (setq targets
            (cons doom-core-dir
                  (cl-remove-if-not
                   (doom-rpartial #'file-in-directory-p doom-emacs-dir)
                   ;; Omit `doom-private-dir', which is always first
                   (let (doom-modules)
                     (load (expand-file-name "test/init" doom-core-dir) nil t)
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
    (setenv "DOOMLOCALDIR" (concat doom-local-dir "test/"))
    (setenv "DOOMDIR" (concat doom-core-dir "test/"))
    (with-temp-buffer
      (print! (start "Bootstrapping test environment, if necessary..."))
      (cl-destructuring-bind (status . output)
          (doom-exec-process
           (doom--emacs-binary)
           "--batch"
           "--eval"
           (prin1-to-string
            `(progn
               (setq user-emacs-directory ,doom-emacs-dir
                     doom-auto-accept t)
               (require 'core ,(locate-library "core"))
               (require 'core-cli)
               (doom-initialize 'force 'noerror)
               (doom-initialize-modules)
               (doom-cli-reload-core-autoloads)
               (when (doom-cli-packages-install)
                 (doom-cli-reload-package-autoloads)))))
        (unless (zerop status)
          (error "Failed to bootstrap unit tests"))))
    (with-temp-buffer
      (dolist (file files)
        (if (doom-file-cookie-p file "if" t)
            (cl-destructuring-bind (_status . output)
                (apply #'doom-exec-process
                       (doom--emacs-binary)
                       "--batch"
                       "-l" (concat doom-core-dir "core.el")
                       "-l" (concat doom-core-dir "test/helpers.el")
                       (append (when (file-in-directory-p file doom-modules-dir)
                                 (list "-f" "doom-initialize-core"))
                               (list "-l" file
                                     "-f" "buttercup-run")))
              (insert (replace-regexp-in-string ansi-color-control-seq-regexp "" output))
              (push file read-files))
          (print! (info "Ignoring %s" (relpath file)))))
      (let ((total 0)
            (total-failed 0)
            (i 0))
        (print! "\n----------------------------------------\nTests finished")
        (print-group!
         (goto-char (point-min))
         (while (re-search-forward "^Ran \\([0-9]+\\) specs, \\([0-9]+\\) failed," nil t)
           (let ((ran (string-to-number (match-string 1)))
                 (failed (string-to-number (match-string 2))))
             (when (> failed 0)
               (terpri)
               (print! (warn "(%s) Failed %d/%d tests")
                       (path (nth i read-files))
                       failed ran)
               (save-excursion
                 (print-group!
                  (print!
                   "%s" (string-trim
                         (buffer-substring
                          (match-beginning 0)
                          (dotimes (_ failed (point))
                            (search-backward "========================================"))))))))
             (cl-incf total ran)
             (cl-incf total-failed failed)
             (cl-incf i))))
        (terpri)
        (if (= total-failed 0)
            (print! (success "Ran %d tests successfully." total total-failed))
          (print! (error "Ran %d tests, %d failed") total total-failed)
          (kill-emacs 1)))
      t)))
