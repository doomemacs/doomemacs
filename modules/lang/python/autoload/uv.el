;;; lang/python/autoload/uv.el -*- lexical-binding: t; -*-
;;;###if (modulep! +uv)

;;;###autoload
(defvar +python--uv-project nil)

;;;###autoload
(defvar +python--uv-version nil)

;;;###autoload
(defun +python-uv-mode-set-auto-h ()
  "Set pyenv-mode version from buffer-local variable."
  (when (derived-mode-p 'python-mode 'python-ts-mode)
    (unless (local-variable-p '+python--uv-project)
      (setq-local +python--uv-project
                  (uv-mode-full-path (uv-mode-root))))
    (if (and +python--uv-project
             (file-exists-p +python--uv-project))
        (unless (eq +python--uv-project (getenv "VIRTUAL_ENV"))
          (setq-local +python--uv-version (uv-mode-version))
          (uv-mode-set))
      (kill-local-variable '+python--uv-project)
      (kill-local-variable '+python--uv-version)
      (uv-mode-unset))))
