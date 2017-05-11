;;; ui/doom/autoload/doom.el

;;;###autoload
(defun +doom/reset-theme ()
  "Reset the color theme currently in use."
  (interactive)
  (let ((theme (or (car-safe custom-enabled-themes) +doom-theme)))
    (when theme
      (mapc #'disable-theme custom-enabled-themes))
    (load-theme theme t)))

