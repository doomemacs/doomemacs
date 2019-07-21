;;; core/cli/test.el -*- lexical-binding: t; -*-

(def-command! test ()
  "Run Doom unit tests.")


;;
;;; Library

(defun doom-run-tests ()
  "Discover and load test files, then run all defined suites.

Takes directories as command line arguments, defaulting to the
current directory."
  (let ((dirs nil)
        (patterns nil)
        (args command-line-args-left)
        (doom-modules (doom-modules)))
    (doom-initialize-autoloads doom-autoload-file)
    (doom-initialize-autoloads doom-package-autoload-file)
    (while args
      (cond
       ;; ((member (car args) '("--traceback"))
       ;;  (when (not (cdr args))
       ;;    (error "Option requires argument: %s" (car args)))
       ;;  ;; Make sure it's a valid style by trying to format a dummy
       ;;  ;; frame with it
       ;;  (buttercup--format-stack-frame '(t myfun 1 2) (intern (cadr args)))
       ;;  (setq buttercup-stack-frame-style (intern (cadr args)))
       ;;  (setq args (cddr args)))
       ;; ((member (car args) '("-p" "--pattern"))
       ;;  (when (not (cdr args))
       ;;    (error "Option requires argument: %s" (car args)))
       ;;  (push (cadr args) patterns)
       ;;  (setq args (cddr args)))
       ;; ((member (car args) '("-c" "--no-color"))
       ;;  (setq buttercup-color nil)
       ;;  (setq args (cdr args)))
       (t
        (push (car args) dirs)
        (setq args (cdr args)))))
    (setq command-line-args-left nil)
    (dolist (dir (or dirs '(".")))
      (setq dir (if (string= dir "core")
                    doom-core-dir
                  (expand-file-name dir doom-modules-dir)))
      (let ((test-dir (expand-file-name "test" dir)))
        (when (or (string= dir doom-core-dir)
                  (cl-destructuring-bind (category . module)
                      (or (doom-module-from-path dir)
                          (cons nil nil))
                    (and category module (doom-module-p category module))))
          (dolist (file (nreverse (doom-glob test-dir "test-*.el")))
            (when (doom-file-cookie-p file)
              (load file nil t))))))
    (when patterns
      (dolist (spec (buttercup--specs buttercup-suites))
        (let ((spec-full-name (buttercup-spec-full-name spec)))
          (unless (cl-dolist (p patterns)
                    (when (string-match p spec-full-name)
                      (cl-return t)))
            (setf (buttercup-spec-function spec)
                  (lambda () (signal 'buttercup-pending "SKIPPED")))))))
    (buttercup-run)))


;;
;; Test library

(defmacro insert!! (&rest text)
  "Insert TEXT in buffer, then move cursor to last {0} marker."
  `(progn
     (insert ,@text)
     (when (search-backward "{0}" nil t)
       (replace-match "" t t))))
