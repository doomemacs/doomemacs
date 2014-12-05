
;; String Defuns ;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(after "s"
  (defun s-count-lines (s)
    "Get number of lines in a string"
    (length (s-lines s))))

;; File Defuns ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(after "f"
  (defmacro f--exists? (file dir)
    `(f-exists? (expand-file-name ,file ,dir))))

;;;###autoload
(after "projectile"
  (defun my--project-root (&optional force-pwd)
    (if (and (not force-pwd)
             (projectile-project-p))
        (projectile-project-root)
      default-directory)))

;; Misc Defuns ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun what-face (pos)
  "Tells you the name of the face (point) is on."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;;;###autoload
(defun what-col ()
  (interactive)
  (message "Column %d" (current-column)))

;;;###autoload
(defun what-bindings (key)
  (list
   (minor-mode-key-binding key)
   (local-key-binding key)
   (global-key-binding key)))
