;;; ui/doom/autoload/doom.el

;;;###autoload
(defun +doom/reset-theme ()
  "Reset the color theme currently in use."
  (interactive)
  (let ((theme (or (car-safe custom-enabled-themes) +doom-theme)))
    (when theme
      (mapc #'disable-theme custom-enabled-themes))
    (load-theme theme t)
    (+doom|refresh-bright-buffers)))

;;;###autoload
(defun +doom|restore-bright-buffers (&rest _)
  "Restore `doom-buffer-mode' in buffers when `persp-mode' loads a session."
  (dolist (buf (persp-buffer-list))
    (with-current-buffer buf
      (+doom|buffer-mode-on))))

;;;###autoload
(defun +doom|refresh-bright-buffers ()
  "Refresh `doom-buffer-mode', in case of graphical glitches."
  (dolist (win (window-list))
    (when (buffer-local-value 'doom-buffer-mode (window-buffer win))
      (with-selected-window win
        (doom-buffer-mode -1)
        (doom-buffer-mode +1)))))
