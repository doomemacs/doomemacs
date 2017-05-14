;;; popups.el

;;;###autoload
(defun doom-popup-p (&optional target)
  "Return TARGET (a window) if TARGET (a window or buffer) is a popup. Uses
current window if omitted."
  (when-let (target (or target (selected-window)))
    (cond ((bufferp target)
           (and (buffer-local-value 'doom-popup-mode target)
                (get-buffer-window target)))
          ((windowp target)
           (and (window-parameter target 'popup)
                target)))))

;;;###autoload
(defun doom-popup-buffer (buffer &rest plist)
  "Display BUFFER in a shackle popup. See `shackle-rules' for possible rules."
  (declare (indent defun))
  (unless (bufferp buffer)
    (error "%s is not a valid buffer" buffer))
  (setq plist (append plist (shackle-match buffer)))
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
  (cl-remove-if-not #'doom-popup-p (window-list)))

;;;###autoload
(defun doom/popup-restore ()
  "Restore the last open popups. If the buffers have been killed, and
represented real files, they will be restored. Special buffers or buffers with
non-nil :autokill properties will not be.

Returns t if popups were restored, nil otherwise."
  (interactive)
  (unless doom-popup-history
    (error "No popups to restore"))
  (let (any-p)
    (dolist (spec doom-popup-history)
      (let ((buffer (get-buffer (car spec)))
            (path   (plist-get (cdr spec) :file))
            (rules  (plist-get (cdr spec) :rules)))
        (when (and (not buffer) path)
          (setq buffer (find-file-noselect path t)))
        (when (and buffer (apply #'doom-popup-buffer buffer rules) (not any-p))
          (setq any-p t))))
    (when any-p
      (setq doom-popup-history '()))
    any-p))

;;;###autoload
(defun doom/popup-toggle ()
  "Toggle popups."
  (interactive)
  (when (doom-popup-p)
    (if doom-popup-other-window
        (select-window doom-popup-other-window)
      (other-window 1)))
  (if (doom-popup-windows)
      (doom/popup-close-all t)
    (doom/popup-restore)))

;;;###autoload
(defun doom/popup-close (&optional window)
  "Find and close WINDOW if it's a popup. If WINDOW is omitted, defaults to
`selected-window'. The contained buffer is buried, unless it has an :autokill
property."
  (interactive)
  (when-let (window (doom-popup-p window))
    (delete-window window)))

;;;###autoload
(defun doom/popup-close-all (&optional force-p)
  "Closes all open popups. If FORCE-P is non-nil, or this function is called
interactively, it will close all popups without question. Otherwise, it will
only close popups that have an :autoclose property in their rule (see
`shackle-rules')."
  (interactive)
  (let ((orig-win (selected-window)))
    (when-let (popups (doom-popup-windows))
      (setq doom-popup-history (mapcar #'doom--popup-data popups))
      (let (doom-popup-remember-history)
        (dolist (window popups)
          (let ((rules (window-parameter window 'popup)))
            (when (or force-p
                      (called-interactively-p 'interactive)
                      (and (plist-member rules :autoclose)
                           (plist-get rules :autoclose)))
              (delete-window window))))))))

;;;###autoload
(defun doom/popup-close-maybe ()
  "Close the current popup *if* its window doesn't have a noesc parameter."
  (interactive)
  (let ((window (selected-window)))
    (if (plist-get doom-popup-rules :noesc)
        (call-interactively (if (featurep 'evil)
                                #'evil-force-normal-state
                              #'keyboard-escape-quit))
      (delete-window window))))

;;;###autoload
(defun doom/popup ()
  "Display currently selected buffer in a popup window."
  (interactive)
  (doom-popup-buffer (current-buffer) :align t :autokill t))

(defun doom--popup-data (window)
  (let ((buffer (window-buffer window)))
    (when buffer
      `(,(buffer-name buffer)
        :file  ,(buffer-file-name buffer)
        :rules ,(window-parameter window 'popup)))))
