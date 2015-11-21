;;; defuns-window.el --- library for acting on windows

;;;###autoload
(defun narf/evil-window-split ()
  (interactive)
  (call-interactively 'evil-window-split)
  (evil-window-down 1))

;;;###autoload
(defun narf/evil-window-vsplit ()
  (interactive)
  (call-interactively 'evil-window-vsplit)
  (evil-window-right 1))

(defun narf--evil-swap-windows (a b)
  (let ((a-buffer (window-buffer a))
        (b-buffer (window-buffer b)))
    (with-selected-window b
      (switch-to-buffer a-buffer))
    (with-selected-window a
      (switch-to-buffer b-buffer))
    (select-window b)))

(defun narf--evil-window-move (direction)
  "Move current window to the next window in DIRECTION. If there are no windows
there and there is only one window, split in that direction and place this
window there. If there are no windows and this isn't the only window, use
evil-window-move-* (e.g. `evil-window-move-far-left')"
  (let* ((this-window (get-buffer-window))
         (this-buffer (current-buffer))
         (that-window (windmove-find-other-window direction nil this-window))
         (that-buffer (window-buffer that-window)))
    (if (and (not that-window) (not (one-window-p t)))
        (funcall (case direction
                   ('left 'evil-window-move-far-left)
                   ('right 'evil-window-move-far-right)
                   ('up 'evil-window-move-very-top)
                   ('down 'evil-window-move-very-bottom)))
      (unless that-window
        (setq that-window
              (split-window this-window nil (cond ((eq direction 'up) 'above)
                                                  ((eq direction 'down) 'below)
                                                  (t direction))))
        (with-selected-window that-window
          (switch-to-buffer "*scratch*"))
        (setq that-buffer (window-buffer that-window)))
      (with-selected-window this-window
        (switch-to-buffer that-buffer))
      (with-selected-window that-window
        (switch-to-buffer this-buffer))
      (select-window that-window))))

;;;###autoload
(defun narf/evil-window-move-left ()
  "See `narf--evil-window-move'"
  (interactive)
  (narf--evil-window-move 'left))
;;;###autoload
(defun narf/evil-window-move-down ()
  "See `narf--evil-window-move'"
  (interactive)
  (narf--evil-window-move 'down))
;;;###autoload
(defun narf/evil-window-move-up ()
  "See `narf--evil-window-move'"
  (interactive)
  (narf--evil-window-move 'up))
;;;###autoload
(defun narf/evil-window-move-right ()
  "See `narf--evil-window-move'"
  (interactive)
  (narf--evil-window-move 'right))

(provide 'defuns-window)
;;; defuns-window.el ends here
