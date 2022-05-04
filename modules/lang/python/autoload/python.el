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
            ((when-let (bin (projectile-locate-dominating-file default-directory "bin/python"))
               (setq-local doom-modeline-python-executable (expand-file-name "bin/python" bin))))
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

;;;###autoload
(defun +python/search-venv-in-directory (directory)
  "Search for .venv-like in directory.

Accepts .venv/ directory, .venv symlink, .venv file containing
path to a directory relative the directory, .venv-* directory or
symlink. The latest .venv-* is used i.e. .venv-py3.10 is prefered
to .venv-py2.7.

Returns venv or nil if no .venv in this directory."
  (let ((bare-venv (f-canonical (f-expand ".venv" directory))))
    ;; First, try .venv
    (if (f-exists? bare-venv)
        ;; Read .venv file
        (if (f-file? bare-venv)
            (let ((venv-content (f-canonical (f-expand (string-trim (f-read bare-venv)) directory))))
              (if (f-exists? venv-content)
                  ;; Read venv from bare .venv
                  venv-content
                (progn
                  (message "%s contains bad venv: %s" bare-venv venv-content)
                  nil)))
          ;; Found bare .venv directory
          bare-venv)
      ;; else search .venv-*
      (let ((first-venv (car (sort (f-glob ".venv-*" directory) 'string>))))
        (if first-venv
            ;; First .venv-* found
            first-venv
          ;; No venv found in directory.
          nil)))))
