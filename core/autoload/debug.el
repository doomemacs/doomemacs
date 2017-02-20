;;; debug.el
(provide 'core-lib-debug)

;;;###autoload
(defun doom/what-face (pos)
  "Tells you the name of the face (point) is on."
  (interactive "d")
  (let ((hl-line-p (bound-and-true-p hl-line-mode)))
    (if hl-line-p (hl-line-mode -1))
    (let ((face (or (get-char-property (point) 'read-face-name)
                    (get-char-property (point) 'face))))
      (if face (message "Face: %s" face) (message "No face at %d" pos)))
    (if hl-line-p (hl-line-mode 1))))

;;;###autoload
(defun doom/what-col ()
  (interactive)
  (message "Column %d" (current-column)))

;;;###autoload
(defun doom/what-bindings (key)
  (list
   (minor-mode-key-binding key)
   (local-key-binding key)
   (global-key-binding key)))

;;;###autoload
(defun doom/what-major-mode ()
  (interactive)
  (message "Mode: %s" major-mode))

