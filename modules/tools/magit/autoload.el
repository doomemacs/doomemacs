;;; tools/magit/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +magit/quit (&optional _kill-buffer)
  "Clean up magit buffers after quitting `magit-status'."
  (interactive)
  (let ((buffers (magit-mode-get-buffers)))
    (magit-restore-window-configuration)
    (mapc #'+magit--kill-buffer buffers)))

(defun +magit--kill-buffer (buf)
  "TODO"
  (when (and (bufferp buf) (buffer-live-p buf))
    (let ((process (get-buffer-process buf)))
      (if (not (processp process))
          (kill-buffer buf)
        (with-current-buffer buf
          (if (process-live-p process)
              (run-with-timer 5 nil #'+magit--kill-buffer buf)
            (kill-process process)
            (kill-buffer buf)))))))

