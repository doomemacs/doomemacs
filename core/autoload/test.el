;;; core/autoload/test.el -*- lexical-binding: t; -*-

;;;###autoload
(defmacro def-test! (name &rest body)
  "Define a namespaced ERT test."
  (declare (indent defun) (doc-string 2))
  (unless (plist-get body :disabled)
    `(ert-deftest
         ,(cl-loop with path = (file-relative-name (file-name-sans-extension load-file-name)
                                                   doom-emacs-dir)
                   for (rep . with) in '(("/test/" . "/") ("/" . ":"))
                   do (setq path (replace-regexp-in-string rep with path t t))
                   finally return (intern (format "%s::%s" path name))) ()
       ()
       ,@body)))

;;;###autoload
(defun doom-run-tests (&optional modules)
  "Run all loaded tests, specified by MODULES (a list of module cons cells) or
command line args following a double dash (each arg should be in the
'module/submodule' format).

If neither is available, run all tests in all enabled modules."
  (interactive) ; must be interactive to be run from batch
  ;; FIXME Refactor this
  (condition-case-unless-debug ex
      (let (targets)
        ;; ensure DOOM is initialized
        (let (noninteractive)
          (load (expand-file-name "init.el" user-emacs-directory) nil t))
        (remove-hook 'doom-init-hook #'doom--display-benchmark)
        ;; collect targets
        (cond ((and command-line-args-left
                    (equal (car command-line-args-left) "--"))
               (cl-loop for arg in (cdr argv)
                        if (equal arg "core")
                        do (push (expand-file-name "test/" doom-core-dir) targets)
                        else
                        collect
                        (cl-destructuring-bind (car &optional cdr) (split-string arg "/" t)
                          (cons (intern (concat ":" car))
                                (and cdr (intern cdr))))
                        into args
                        finally do (setq modules args
                                         command-line-args-left nil)))

              (modules
               (unless (cl-loop for module in modules
                                unless (and (consp module)
                                            (keywordp (car module))
                                            (symbolp (cdr module)))
                                return t)
                 (error "Expected a list of cons, got: %s" modules)))

              (t
               (let ((noninteractive t)
                     doom-modules)
                 (load (expand-file-name "init.test.el" user-emacs-directory) nil t)
                 (setq modules (doom--module-pairs)
                       targets (list (expand-file-name "test/" doom-core-dir))))))
        ;; resolve targets to a list of test files and load them
        (cl-loop with targets =
                 (append targets
                         (cl-loop for (module . submodule) in modules
                                  if submodule
                                  collect (doom-module-path module submodule "test/")
                                  else
                                  nconc
                                  (cl-loop with module-name = (substring (symbol-name module) 1)
                                           with module-path = (expand-file-name module-name doom-modules-dir)
                                           for path in (directory-files module-path t "^\\w")
                                           collect (expand-file-name "test/" path))))
                 for dir in targets
                 if (file-directory-p dir)
                 nconc (reverse (directory-files-recursively dir "\\.el$"))
                 into items
                 finally do (quiet! (mapc #'load-file items)))
        ;; run all loaded tests
        (when noninteractive
          (ert-run-tests-batch-and-exit)))
    ('error
     (lwarn 'doom-test :error
            "%s -> %s"
            (car ex) (error-message-string ex)))))
