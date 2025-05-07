;;; lang/python/autoload/pyenv.el -*- lexical-binding: t; -*-
;;;###if (modulep! +pyenv)

;;;###autoload
(defvar +pyenv--version nil)

;;;###autoload
(defun +python-pyenv-mode-set-auto-h ()
  "Set pyenv-mode version from buffer-local variable."
  (when (memq major-mode '(python-mode python-ts-mode))
    (when (not (local-variable-p '+pyenv--version))
      (make-local-variable '+pyenv--version)
      (setq +pyenv--version (+python-pyenv-read-version-from-file)))
    (if +pyenv--version
        (pyenv-mode-set +pyenv--version)
      (pyenv-mode-unset))))

;;;###autoload
(defun +python-pyenv-read-version-from-file ()
  "Read pyenv version from .python-version file."
  (when-let (root-path (projectile-locate-dominating-file default-directory ".python-version"))
    (let* ((file-path (expand-file-name ".python-version" root-path))
           (version
            (with-temp-buffer
              (insert-file-contents-literally file-path)
              (string-trim (buffer-string)))))
      (if (member version (pyenv-mode-versions))
          version  ;; return.
        (message "pyenv: version `%s' is not installed (set by `%s')."
                 version file-path)))))
