;;; lang/python/autoload/python.el -*- lexical-binding: t; -*-

(defvar +python-version-cache (make-hash-table :test 'equal)
  "TODO")

;;;###autoload
(defun +python/repl ()
  "Open the Python REPL."
  (interactive)
  (process-buffer (run-python nil t t)))

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
        (puthash (doom-project-root)
                 (+python--extract-version "Python " (car (process-lines "python" "--version")))
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
                         (doom-project-root))
                     +python-version-cache)
            (+python-version))))

;;;###autoload
(defun +python|update-version-in-all-buffers ()
  "Update `+python-version' in all buffers in `python-mode'."
  (dolist (buffer (doom-buffers-in-mode 'python-mode))
    (setq +python-version-cache (clrhash +python-version-cache))
    (with-current-buffer buffer
      (+python|update-version))))
