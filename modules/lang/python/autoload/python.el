;;; lang/python/autoload/python.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +python/open-repl ()
  "Open the Python REPL."
  (interactive)
  (require 'python)
  (unless python-shell-interpreter
    (user-error "`python-shell-interpreter' isn't set"))
  (pop-to-buffer
   (process-buffer
    (if-let* ((pipenv (+python-executable-find "pipenv"))
              (pipenv-project (pipenv-project-p)))
        (let ((default-directory pipenv-project)
              (python-shell-interpreter-args
               (format "run %s %s"
                       python-shell-interpreter
                       python-shell-interpreter-args))
              (python-shell-interpreter pipenv))
          (run-python nil nil t))
      (run-python nil nil t)))))

;;;###autoload
(defun +python/open-ipython-repl ()
  "Open an IPython REPL."
  (interactive)
  (require 'python)
  (let ((python-shell-interpreter (or (+python-executable-find "ipython") "ipython"))
        (python-shell-interpreter-args (string-join +python-ipython-repl-args " ")))
    (+python/open-repl)))

;;;###autoload
(defun +python/open-jupyter-repl ()
  "Open a Jupyter console."
  (interactive)
  (require 'python)
  (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")
  (let ((python-shell-interpreter (or (+python-executable-find "jupyter") "jupyter"))
        (python-shell-interpreter-args (format "console %s" (string-join +python-jupyter-repl-args " "))))
    (+python/open-repl)))

;;;###autoload
(defun +python-executable-find (exe)
  "TODO"
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
            ((when-let (bin (projectile-locate-dominating-file default-directory "bin/python"))
               (setq-local doom-modeline-python-executable (expand-file-name "bin/python" bin))))
            ((executable-find exe))))))

;;;###autoload
(defun +python/optimize-imports ()
  "organize imports"
  (interactive)
  (pyimport-remove-unused)
  (py-isort-buffer))
