;;; ui.el
(provide 'doom-lib-ui)

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
(defun doom/reset-theme ()
  "Reset the color theme currently in use."
  (interactive)
  (let ((theme (car-safe custom-enabled-themes)))
    (when theme
      (mapc 'disable-theme custom-enabled-themes))
    (load-theme theme t)))

