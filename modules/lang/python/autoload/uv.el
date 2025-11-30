;;; lang/python/autoload/uv.el -*- lexical-binding: t; -*-
;;;###if (modulep! +uv)

;;;###autoload
(defvar +uv--project nil)

;;;###autoload
(defun +python-uv-mode-set-auto-h ()
  "Set uv-mode virtualenv from buffer-local variable."
  (when (memq major-mode '(python-mode python-ts-mode))
    (when (not (local-variable-p '+uv--project))
      (make-local-variable '+uv--project)
      (setq +uv--project (+python-uv-read-project-from-file)))
    (if +uv--project
        (uv-mode-set)
      (uv-mode-unset))))

;;;###autoload
(defun +python-uv-read-project-from-file ()
  "Read uv project root from .venv directory presence."
  (when-let (root-path (projectile-locate-dominating-file default-directory ".venv"))
    (let ((venv-path (expand-file-name ".venv" root-path)))
      (if (file-directory-p venv-path)
          root-path
        (message "uv: .venv directory not found in `%s'." root-path)
        nil))))
