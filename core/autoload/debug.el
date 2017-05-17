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

;;;###autoload
(defun doom/what-col ()
  (interactive)
  (message "Column %d" (current-column)))

;;;###autoload
(defun doom/what-bindings (key)
  (interactive "k")
  (message "minor-mode:\t%s\nlocal:\t\t%s\nglobal:\t\t%s"
           (or (minor-mode-key-binding key) "n/a")
           (or (local-key-binding key) "n/a")
           (or (global-key-binding key) "n/a")))

