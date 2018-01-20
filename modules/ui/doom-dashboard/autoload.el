;;; ui/doom-dashboard/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +doom-dashboard/open (frame)
  "Switch to the dashboard in the current window, of the current FRAME."
  (interactive (list (selected-frame)))
  (with-selected-frame frame
    (switch-to-buffer (doom-fallback-buffer))
    (+doom-dashboard-reload)))

;;;###autoload
(defun +doom-dashboard/next-button ()
  "Jump to the next button after cursor."
  (interactive)
  (ignore-errors (goto-char (next-button (point)))))

;;;###autoload
(defun +doom-dashboard/previous-button ()
  "Jump to the previous button after cursor."
  (interactive)
  (ignore-errors (goto-char (previous-button (point)))))

;;;###autoload
(defun +doom-dashboard/first-button ()
  "Jump to the first button on the dashboard."
  (interactive)
  (goto-char (point-min))
  (+doom-dashboard/next-button))

;;;###autoload
(defun +doom-dashboard/last-button ()
  "Jump to the last button on the dashboard."
  (interactive)
  (goto-char (point-max))
  (+doom-dashboard/previous-button)
  (beginning-of-line-text))
