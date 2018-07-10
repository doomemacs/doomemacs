;;; ui/doom-dashboard/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +doom-dashboard/open (frame)
  "Switch to the dashboard in the current window, of the current FRAME."
  (interactive (list (selected-frame)))
  (with-selected-frame frame
    (switch-to-buffer (+doom-dashboard-initial-buffer))
    (+doom-dashboard-reload t)))

;;;###autoload
(defun +doom-dashboard/forward-button (n)
  "Like `forward-button', but don't wrap."
  (interactive "p")
  (forward-button n nil))

;;;###autoload
(defun +doom-dashboard/backward-button (n)
  "Like `backward-button', but don't wrap."
  (interactive "p")
  (backward-button n nil))
