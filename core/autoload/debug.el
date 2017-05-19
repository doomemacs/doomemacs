;;; debug.el

;;;###autoload
(defun doom/what-face (pos)
  "Lists all faces at point. Overlay faces are <>-delimited."
  (interactive "d")
  (let ((pos (point))
        faces)
    (when-let (face (get-text-property pos 'face))
      (dolist (f (if (listp face) face (list face)))
        (push (propertize (symbol-name f) 'face f) faces)))
    (dolist (ov (overlays-at pos (1+ pos)))
      (let ((face (overlay-get ov 'face)))
        (dolist (f (if (listp face) face (list face)))
          (push (propertize (concat (symbol-name f) "*") 'face f) faces))))

    (message "%s %s"
             (propertize "Faces:" 'face 'font-lock-comment-face)
             (if faces (string-join faces ", ") "n/a"))))

(defun doom-active-minor-modes ()
  "Get a list of active minor-mode symbols."
  (cl-remove-if (lambda (m) (and (boundp m) (symbol-value m)))
                minor-mode-list))

;;;###autoload
(defun doom/what-minor-mode (mode)
  "Get information on an active minor mode. Use `describe-minor-mode' for a
selection of all minor-modes, active or not."
  (interactive
   (list (completing-read "Minor mode: "
                          (doom-active-minor-modes))))
  (describe-minor-mode-from-symbol (intern mode)))
