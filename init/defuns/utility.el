(defun what-face (pos)
  "Tells you the name of the face (point) is on."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun my/append-to-list (list-var elements)
  "Append ELEMENTS to the end of LIST."
  (unless (consp elements)
    (error "ELEMENTS must be a list"))
  (let ((list (symbol-value list-var)))
    (if list
        (setcdr (last list) elements)
      (set list-var elements)))
  (symbol-value list-var))

(defun my/project-root (&optional force-pwd)
  (if (and (not force-pwd)
           (projectile-project-p))
      (projectile-project-root)
    default-directory))

(defmacro f--exists? (file dir)
  `(f-exists? (expand-file-name ,file ,dir)))
