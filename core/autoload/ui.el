;;; ui.el

;;;###autoload
(defun doom/toggle-fullscreen ()
  "Toggle fullscreen Emacs."
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (unless (frame-parameter nil 'fullscreen)
     'fullboth)))

;;;###autoload
(defun doom/toggle-line-numbers ()
  "Toggle `nlinum-mode'."
  (interactive)
  (nlinum-mode (if nlinum-mode -1 +1)))

;;;###autoload
(defun doom/window-zoom ()
  "Maximize and isolate the current buffer. Activate again to undo this. If the
window changes before then, the undo expires."
  (interactive)
  (if (and (one-window-p)
           (assoc ?_ register-alist))
      (jump-to-register ?_)
    (window-configuration-to-register ?_)
    (delete-other-windows)))
