;;; app/twitter/autoload.el

;;;###autoload
(defun =twitter ()
  (interactive)
  (+workspace-switch "Twitter" t)
  (delete-other-windows)
  (call-interactively 'twit)
  (switch-to-buffer (car twittering-initial-timeline-spec-string))
  (dolist (name (cdr twittering-initial-timeline-spec-string))
    (split-window-horizontally)
    (switch-to-buffer name))
  (balance-windows))

;;;###autoload
(defun +twitter/quit ()
  (interactive)
  (when (eq major-mode 'twittering-mode)
    (twittering-kill-buffer)
    (+workspace/close-window-or-workspace)))

;;;###autoload
(defun +twitter/quit-all ()
  (interactive)
  (+workspace/delete "Twitter")
  (dolist (buf (doom-buffers-in-mode 'twittering-mode))
    (with-current-buffer buf
      (twittering-kill-buffer))))

;;;###autoload
(defun +twitter/rerender-all ()
  (interactive)
  (dolist (buf (doom-buffers-in-mode 'twittering-mode))
    (with-current-buffer buf
      (twittering-rerender-timeline-all buf))))
