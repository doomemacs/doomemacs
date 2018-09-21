;;; lang/python/autoload/python.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +python/repl ()
  "Open the Python REPL."
  (interactive)
  (process-buffer (run-python nil t t)))

;;;###autoload
(defun +python-version ()
  "Return the currently installed version of python on your system or active in
the current pipenv.

This is not necessarily aware of env management tools like virtualenv, pyenv or
pipenv, unless those tools have modified the PATH that Emacs picked up when you
started it."
  (let* ((pipenv-dir (pipenv-project-p))
         (default-directory (or pipenv-dir default-directory))
         (command (if pipenv-dir
                      "pipenv run python --version"
                    "python --version"))
         (bin (car (split-string command " "))))
    (unless (executable-find bin)
      (user-error "Couldn't find %s executable in PATH" bin))
    (with-temp-buffer
      (let ((p (apply #'call-process bin nil (current-buffer) nil
                      (cdr (split-string command " " t))))
            (output (string-trim (buffer-string))))
        (unless (zerop p)
          (user-error "'%s' failed: %s" command output))
        (cadr (split-string output " " t))))))
