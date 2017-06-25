;;; ui/doom-dashboard/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +doom-dashboard/next-button ()
  (interactive)
  (ignore-errors (goto-char (next-button (point)))))

;;;###autoload
(defun +doom-dashboard/previous-button ()
  (interactive)
  (ignore-errors (goto-char (previous-button (point)))))

;;;###autoload
(defun +doom-dashboard/first-button ()
  (interactive)
  (goto-char (point-min))
  (+doom-dashboard/next-button))

;;;###autoload
(defun +doom-dashboard/last-button ()
  (interactive)
  (goto-char (point-max))
  (+doom-dashboard/previous-button)
  (beginning-of-line-text))
