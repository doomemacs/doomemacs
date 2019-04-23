;;; ui/doom-dashboard/autoload.el -*- lexical-binding: t; -*-

(defun +doom-dashboard--help-echo ()
  (when-let* ((btn (button-at (point)))
              (msg (button-get btn 'help-echo)))
    (message "%s" msg)))

;;;###autoload
(defun +doom-dashboard/open (frame)
  "Switch to the dashboard in the current window, of the current FRAME."
  (interactive (list (selected-frame)))
  (with-selected-frame frame
    (switch-to-buffer (doom-fallback-buffer))
    (+doom-dashboard-reload t)))

;;;###autoload
(defun +doom-dashboard/forward-button (n)
  "Like `forward-button', but don't wrap."
  (interactive "p")
  (forward-button n nil)
  (+doom-dashboard--help-echo))

;;;###autoload
(defun +doom-dashboard/backward-button (n)
  "Like `backward-button', but don't wrap."
  (interactive "p")
  (backward-button n nil)
  (+doom-dashboard--help-echo))
