;;; ui/doom-dashboard/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +doom-dashboard/open (frame)
  "Switch to the dashboard in the current window, of the current FRAME."
  (interactive (list (selected-frame)))
  (with-selected-frame frame
    (switch-to-buffer (doom-fallback-buffer))))
