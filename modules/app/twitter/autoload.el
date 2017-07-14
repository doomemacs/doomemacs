;;; app/twitter/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun =twitter ()
  (interactive)
  (+workspace-switch "*Twitter*" t)
  (delete-other-windows)
  (condition-case _ex
      (progn
        (call-interactively #'twit)
        (unless (get-buffer (car twittering-initial-timeline-spec-string))
          (error "Failed to open twitter"))
        (switch-to-buffer (car twittering-initial-timeline-spec-string))
        (dolist (name (cdr twittering-initial-timeline-spec-string))
          (split-window-horizontally)
          (switch-to-buffer name))
        (balance-windows))
    ('error
     (+twitter/quit-all))))

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
