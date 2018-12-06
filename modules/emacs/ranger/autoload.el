;;; private/ranger/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +my/dired-setup ()
  (setq dired-omit-verbose nil)
  (make-local-variable 'dired-hide-details-hide-symlink-targets)
  (setq dired-hide-details-hide-symlink-targets nil)

  ;; hide details by default
  (dired-hide-details-mode t)
  ;; omit the .. in dired
  (dired-omit-mode t))
