;;; defuns-debug.el

;;;###autoload
(defun what-face (pos)
  "Tells you the name of the face (point) is on."
  (interactive "d")
  (let ((hl-line-p hl-line-mode))
    (if hl-line-p (hl-line-mode -1))
    (let ((face (or (get-char-property (point) 'read-face-name)
                    (get-char-property (point) 'face))))
      (if face (message "Face: %s" face) (message "No face at %d" pos)))
    (if hl-line-p (hl-line-mode 1))))

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

;;;###autoload
(defun what-major-mode ()
  (interactive)
  (message "Mode: %s" major-mode))

;;;###autoload
(defun what-minor-modes ()
  (interactive)
  (let ((buf (get-buffer-create "*minor-modes*")))
    (with-current-buffer buf
      (insert "Active minor modes:\n + ")
      (insert (s-join "\n + " (-filter
                               (lambda (k) (and k (not (string= k ""))))
                               (mapcar (lambda (mm) (symbol-name (car mm)))
                                       minor-mode-alist)))))
    (popwin:pop-to-buffer buf)))


;;;###autoload (autoload 'narf:echo "defuns-debug" nil t)
(evil-define-command narf:echo (bang message)
  "Display MSG in echo-area without logging it in *Messages* buffer."
  (interactive "<!><a>")
  (let (message-log-max)
    (message "%s%s" (if bang ">> " "") message)))

(provide 'defuns-debug)
;;; defuns-debug.el ends here
