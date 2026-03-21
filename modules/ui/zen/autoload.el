;;; ui/zen/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defalias '+zen/toggle #'writeroom-mode)

;;;###autoload
(defun +zen/toggle-fullscreen ()
  "Toggle `writeroom-mode' fullscreen and delete all other windows.
Invoke again to revert to the window configuration before it was activated."
  (interactive)
  (require 'writeroom-mode)
  (let ((writeroom-global-effects +zen--old-writeroom-global-effects)
        (writeroom-maximize-window t))
    (if writeroom-mode
        (progn
          (set-frame-parameter
           nil 'fullscreen
           (let ((fullscreen-restore (frame-parameter nil 'fullscreen-restore)))
             (if (memq fullscreen-restore '(maximized fullheight fullwidth))
                 fullscreen-restore
               nil)))
          (set-window-configuration (get '+zen/toggle-fullscreen 'last-wconf)))
      (put '+zen/toggle-fullscreen 'last-wconf (current-window-configuration))
      (modify-frame-parameters
       nil `((fullscreen . fullboth)
             (fullscreen-restore . ,(frame-parameter nil 'fullscreen)))))
    (let ((writeroom-global-effects (remq 'writeroom-set-fullscreen writeroom-global-effects)))
      (call-interactively #'+zen/toggle))))

;;; autoload.el ends here
