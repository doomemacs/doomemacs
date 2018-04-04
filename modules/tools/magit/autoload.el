;;; tools/magit/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +magit/quit (&optional _kill-buffer)
  "TODO"
  (interactive)
  (magit-restore-window-configuration)
  (mapc #'kill-buffer (doom-buffers-in-mode 'magit-mode (buffer-list) t)))
