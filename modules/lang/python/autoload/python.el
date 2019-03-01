;;; lang/python/autoload/python.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +python/open-repl ()
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
        (python-shell-interpreter-args (string-join +python-ipython-repl-args " ")))
    (+python/open-repl)))

;;;###autoload
(defun +python/open-jupyter-repl ()
  "Open a Jupyter console."
  (interactive)
  (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")
  (let ((python-shell-interpreter "jupyter")
        (python-shell-interpreter-args (format "console %s" (string-join +python-jupyter-repl-args " "))))
    (+python/open-repl)))
