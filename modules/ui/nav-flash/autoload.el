;;; ui/nav-flash/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +doom*blink-cursor-maybe (orig-fn &rest args)
  "Blink current line if the window has moved."
  (let ((point (save-excursion (goto-char (window-start))
                               (point-marker))))
    (apply orig-fn args)
    (unless (equal point
                   (save-excursion (goto-char (window-start))
                                   (point-marker)))
      (+doom/blink-cursor))))

;;;###autoload
(defun +doom/blink-cursor (&rest _)
  "Blink current line using `nav-flash'."
  (interactive)
  (unless (minibufferp)
    (nav-flash-show)
    ;; only show in the current window
    (overlay-put compilation-highlight-overlay 'window (selected-window))))
