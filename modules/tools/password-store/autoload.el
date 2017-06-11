;;; tools/password-store/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +pass/find ()
  (interactive)
  (let ((default-directory (expand-file-name "~/.password-store")))
    (call-interactively #'projectile-find-file)))

;;;###autoload
(defun +pass/browse ()
  (interactive)
  (let ((default-directory (expand-file-name "~/.password-store")))
    (call-interactively #'find-file)))
