;;; tools/magit/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +magit/quit (&optional _kill-buffer)
  "TODO"
  (interactive)
  (magit-restore-window-configuration)
  (cl-loop for buf in (doom-buffers-in-mode 'magit-mode (buffer-list) t)
           unless (eq (buffer-local-value 'major-mode buf) 'magit-process-mode)
           do (kill-buffer buf)))
