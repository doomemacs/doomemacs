;;; lang/python/autoload/python.el -*- lexical-binding: t; -*-

(defvar +python-version-cache (make-hash-table :test 'equal)
  "TODO")

;;;###autoload
(defun +python/repl ()
  "Open the Python REPL."
  (interactive)
  (unless python-shell-interpreter
    (user-error "`python-shell-interpreter' isn't set"))
  (pop-to-buffer
   (process-buffer
    (if-let* ((pipenv (executable-find "pipenv"))
              (pipenv-project (pipenv-project-p)))
        (let ((default-directory pipenv-project)
              (python-shell-interpreter-args
               (format "run %s %s"
                       python-shell-interpreter
                       python-shell-interpreter-args))
              (python-shell-interpreter pipenv))
          (run-python nil t t))
      (run-python nil t t)))))

;;;###autoload
(defun +python/open-ipython-repl ()
  "Open an IPython REPL."
  (interactive)
  (let ((python-shell-interpreter "ipython")
        (python-shell-interpreter-args +python-jupyter-repl-args))
    (+python/repl)))

;;;###autoload
(defun +python/open-jupyter-repl ()
  "Open a Jupyter console."
  (interactive)
  (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")
  (let ((python-shell-interpreter "jupyter")
        (python-shell-interpreter-args (format "console %s" +python-jupyter-repl-args)))
    (+python/repl)))

(defun +python--extract-version (prefix str)
  (when str
    (format "%s%s" prefix (cadr (split-string str " ")))))

;;;###autoload
(defun +python-version ()
  "Return the currently installed version of python on your system or active in
the current pipenv.

This is not necessarily aware of env management tools like virtualenv, pyenv or
pipenv, unless those tools have modified the PATH that Emacs picked up when you
started it."
  (condition-case _
      (if-let* ((proot (and (fboundp 'pipenv-project-p)
                            (pipenv-project-p))))
          (let* ((default-directory proot)
                 (v (car (process-lines "pipenv" "run" "python" "--version"))))
            (puthash proot
                     (+python--extract-version "Pipenv " v)
                     +python-version-cache))
        (puthash (or (doom-project-root) default-directory)
                 (+python--extract-version
                  "Python "
                  (car (process-lines python-shell-interpreter "--version")))
                 +python-version-cache))
    (error "Python")))


;;
;; Hooks

;;;###autoload
(defun +python|update-version (&rest _)
  "Update `+python--version' by consulting `+python-version' function."
  (setq +python--version
        (or (gethash (or (and (fboundp 'pipenv-project-p)
                              (pipenv-project-p))
                         (doom-project-root)
                         default-directory)
                     +python-version-cache)
            (+python-version))))

;;;###autoload
(defun +python|update-version-in-all-buffers (&rest _)
  "Update `+python-version' in all buffers in `python-mode'."
  (dolist (buffer (doom-buffers-in-mode 'python-mode))
    (setq +python-version-cache (clrhash +python-version-cache))
    (with-current-buffer buffer
      (+python|update-version))))
