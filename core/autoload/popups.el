;;; popups.el
(provide 'doom-lib-popups)

;;;###autoload
(defun doom-popup-p (&optional window)
  "Return t if WINDOW is a popup. Uses current window if WINDOW is omitted."
  (let ((window (or window (selected-window))))
    (and window (buffer-local-value 'doom-popup-mode (window-buffer window)))))

;;;###autoload
(defun doom-popup-buffer (buffer &rest plist)
  "Display BUFFER in a shackle popup. See `shackle-rules' for possible rules."
  (unless (bufferp buffer)
    (error "%s is not a valid buffer" buffer))
  (when (and plist (not (plist-member plist :align)))
    (plist-put plist :align t))
  (shackle-display-buffer
   buffer
   nil (or plist (shackle-match buffer))))

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
  "Restore the last popups."
  (interactive)
  (unless doom-popup-history
    (error "No popups to restore"))
  (dolist (spec doom-popup-history)
    (let ((buffer (get-buffer (car spec)))
          (path (plist-get (cdr spec) :file))
          (rules (plist-get (cdr spec) :rules)))
      (when (and (not buffer) path)
        (setq buffer (find-file-noselect path t)))
      (when buffer
        (apply 'doom-popup-buffer buffer rules))))
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
    (when (doom-popup-p window)
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
      (let (doom-popup-remember-history)
        (mapc 'delete-window popups)))))

;;;###autoload
(defun doom/popup-close-maybe ()
  "Close the current popup *if* its window doesn't have a noesc parameter."
  (interactive)
  (let ((window (selected-window)))
    (if (window-parameter window 'noesc)
        (call-interactively (if (featurep 'evil)
                                'evil-force-normal-state
                              'keyboard-escape-quit))
      (delete-window window))))

(defun doom--popup-data (window)
  (let ((buffer (window-buffer window)))
    `(,(buffer-name buffer)
      :file  ,(buffer-file-name buffer)
      :rules ,(window-parameter window 'popup))))
