;;; popups.el
(provide 'doom-lib-popups)

;;;###autoload
(defun doom-popup-p (&optional window)
  "Return t if WINDOW is a popup. Uses current window if WINDOW is omitted."
  (let ((window (or window (selected-window))))
    (and window
         (window-parameter window 'popup))))

;;;###autoload
(defun doom-popup-buffer (buffer &rest plist)
  "Display BUFFER in a shackle popup. See `shackle-rules' for possible rules."
  (let* ((buffer-name (cond ((stringp buffer) buffer)
                            ((bufferp buffer) (buffer-name buffer))
                            (t (error "Not a valid buffer"))))
         (buffer (get-buffer-create buffer-name)))
    (unless (doom-popup-p)
      (setq doom-popup-other-window (selected-window)))
    (when (and plist (not (plist-member plist :align)))
      (plist-put plist :align shackle-default-alignment))
    (shackle-display-buffer
     buffer
     nil (or plist (shackle-match buffer-name)))))

;;;###autoload
(defun doom-popup-file (file &rest plist)
  "Display FILE in a shackle popup, with PLIST rules. See `shackle-rules' for
possible rules."
  (unless (file-exists-p file)
    (user-error "Can't display file in popup, it doesn't exist: %s" file))
  (doom-popup-buffer (find-file-noselect file t) plist))

;;;###autoload
(defun doom-popup-windows ()
  "Get a list of open poups."
  (-filter 'doom-popup-p (window-list)))

;;;###autoload
(defun doom/popup-restore ()
  "Restore the last popup."
  (interactive)
  (unless doom-popup-history
    (error "No popups to restore"))
  (dolist (spec doom-popup-history)
    (let ((buffer (get-buffer (car spec)))
          (path (plist-get spec :file)))
      (when (and (not buffer) path)
        (setq buffer (find-file-noselect path t)))
      (when buffer
        (doom-popup-buffer buffer (plist-get spec :rules)))))
  (setq doom-popup-history '()))

;;;###autoload
(defun doom/popup-restore-or-switch ()
  (interactive)
  (let (popups)
    (cond ((doom-popup-p)
           (unless (doom-visible-windows)
             (user-error "No non-popups available"))
           (other-window 1))
          ((setq popups (doom-popup-windows))
           (select-window (car popups)))
          (t
           (doom/popup-restore)))))

;;;###autoload
(defun doom/popup-close (&optional window)
  "Find and close WINDOW if it's a popup. If WINDOW is omitted, it will use
`selected-window'. The contained buffer is buried."
  (interactive)
  (let ((window (or window (selected-window))))
    (when (and (doom-popup-p window)
               (window-live-p window))
      (with-selected-window window
        (when (called-interactively-p 'interactive)
          (run-hooks 'doom-popup-close-hook))
        (doom-popup-mode -1)
        (when doom-popup-remember-history
          (setq doom-popup-history (list (doom--popup-data window)))))
      (delete-window window))))

;;;###autoload
(defun doom/popup-close-all ()
  "Closes all open popups. If DONT-KILL is non-nil, don't kill their buffers."
  (interactive)
  (let* ((orig-win (selected-window))
         (popups (--filter (and (doom-popup-p it) (not (eq it orig-win)))
                           (window-list))))
    (when popups
      (setq doom-popup-history (mapcar 'doom--popup-data (doom-popup-windows)))
      (run-hooks 'doom-popup-close-hook)
      (let (doom-popup-remember-history)
        (mapc 'doom/popup-close popups)))))

;;;###autoload
(defun doom/popup-close-maybe ()
  "Close the current popup *if* its window doesn't have a noesc parameter."
  (interactive)
  (if (window-parameter (selected-window) 'noesc)
      (call-interactively (if (featurep 'evil)
                              'evil-force-normal-state
                            'keyboard-escape-quit))
    (doom/popup-close)))

(defun doom--popup-data (window)
  (let ((buffer (window-buffer window)))
    `(,(buffer-name buffer)
      :file  ,(buffer-file-name buffer)
      :rules ,(window-parameter window 'popup))))
