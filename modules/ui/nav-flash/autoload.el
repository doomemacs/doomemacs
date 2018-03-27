;;; ui/nav-flash/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +nav-flash*blink-cursor-maybe (orig-fn &rest args)
  "Blink current line if the window has moved."
  (if (or (not (window-start))
          (derived-mode-p 'term-mode))
      (apply orig-fn args)
    (let* ((win-beg (window-start))
           (point (save-excursion (goto-char win-beg) (point-marker))))
      (apply orig-fn args)
      (unless (equal point (save-excursion (goto-char win-beg) (point-marker)))
        (+nav-flash/blink-cursor)))))

;;;###autoload
(defun +nav-flash/blink-cursor (&rest _)
  "Blink current line using `nav-flash'."
  (interactive)
  (unless (or (minibufferp)
              (memq this-command '(mouse-set-point evil-mouse-drag-region)))
    (nav-flash-show)
    ;; only show in the current window
    (overlay-put compilation-highlight-overlay 'window (selected-window))))
