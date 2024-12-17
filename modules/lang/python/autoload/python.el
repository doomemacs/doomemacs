;;; lang/python/autoload/python.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +python-executable-find (exe)
  "Resolve the path to the EXE executable.
Tries to be aware of your active conda/pipenv/virtualenv environment, before
falling back on searching your PATH."
  (if (file-name-absolute-p exe)
      (and (file-executable-p exe)
           exe)
    (let ((exe-root (format "bin/%s" exe)))
      (cond ((when python-shell-virtualenv-root
               (let ((bin (expand-file-name exe-root python-shell-virtualenv-root)))
                 (if (file-exists-p bin) bin))))
            ((when (require 'conda nil t)
               (let ((bin (expand-file-name (concat conda-env-current-name "/" exe-root)
                                            (conda-env-default-location))))
                 (if (file-executable-p bin) bin))))
            ((when-let (bin (projectile-locate-dominating-file default-directory exe-root))
               (setq-local doom-modeline-python-executable (expand-file-name exe-root bin))))
            ((executable-find exe))))))

;;;###autoload
(defun +python/open-repl ()
  "Open the Python REPL."
  (interactive)
  (require 'python)
  (unless python-shell-interpreter
    (user-error "`python-shell-interpreter' isn't set"))
  (pop-to-buffer
   (process-buffer
    (let ((dedicated (bound-and-true-p python-shell-dedicated)))
      (if-let* ((pipenv (+python-executable-find "pipenv"))
                (pipenv-project (pipenv-project-p)))
          (let ((default-directory pipenv-project)
                (python-shell-interpreter-args
                 (format "run %s %s"
                         python-shell-interpreter
                         python-shell-interpreter-args))
                (python-shell-interpreter pipenv))
            (run-python nil dedicated t))
        (run-python nil dedicated t))))))

;;;###autoload
(defun +python/open-ipython-repl ()
  "Open an IPython REPL."
  (interactive)
  (require 'python)
  (let ((python-shell-interpreter
         (or (+python-executable-find (car +python-ipython-command))
             "ipython"))
        (python-shell-interpreter-args
         (string-join (cdr +python-ipython-command) " ")))
    (+python/open-repl)))

;;;###autoload
(defun +python/open-jupyter-repl ()
  "Open a Jupyter console."
  (interactive)
  (require 'python)
  (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")
  (let ((python-shell-interpreter
         (or (+python-executable-find (car +python-jupyter-command))
             "jupyter"))
        (python-shell-interpreter-args
         (string-join (cdr +python-jupyter-command) " ")))
    (+python/open-repl)))

;;;###autoload
(defun +python/optimize-imports ()
  "organize imports"
  (interactive)
  (pyimport-remove-unused)
  (py-isort-buffer))
