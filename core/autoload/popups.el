;;; core/autoload/popups.el -*- lexical-binding: t; -*-

(defvar doom-popup-remember-history)

;;;###autoload
(defun doom-popup-p (&optional target)
  "Return TARGET (a window) if TARGET (a window or buffer) is a popup. Uses
current window if omitted."
  (when-let (target (or target (selected-window)))
    (cond ((bufferp target)
           (buffer-local-value 'doom-popup-mode target))
          ((windowp target)
           (and (window-parameter target 'popup)
                target)))))

;;;###autoload
(defun doom-popup-buffer (buffer &rest plist)
  "Display BUFFER in a shackle popup. See `shackle-rules' for possible rules.
Returns the new popup window."
  (declare (indent defun))
  (unless (bufferp buffer)
    (error "%s is not a valid buffer" buffer))
  (setq plist (append plist (shackle-match buffer)))
  (shackle-display-buffer
   buffer
   nil (or plist (shackle-match buffer))))

;;;###autoload
(defun doom-popup-switch-to-buffer (buffer)
  "Switch the current (or closest) pop-up window to BUFFER."
  (unless (doom-popup-p)
    (let ((popups (doom-popup-windows)))
      (unless popups
        (error "No popups to switch"))
      (select-window (car popups))))
  (set-window-dedicated-p nil nil)
  (switch-to-buffer buffer nil t)
  (prog1 (selected-window)
    (set-window-dedicated-p nil t)))

;;;###autoload
(defun doom-popup-file (file &rest plist)
  "Display FILE in a shackle popup, with PLIST rules. See `shackle-rules' for
possible rules."
  (unless (file-exists-p file)
    (user-error "Can't display file in popup, it doesn't exist: %s" file))
  (doom-popup-buffer (find-file-noselect file t) plist))

;;;###autoload
(defun doom-popup-windows ()
  "Get a list of open pop up windows."
  (cl-remove-if-not #'doom-popup-p doom-popup-windows))

;;;###autoload
(defun doom/popup-restore ()
  "Restore the last open popups. If the buffers have been killed, and
represented real files, they will be restored. Dead special buffers or buffers
with non-nil :autokill properties will not be.

Returns t if popups were restored, nil otherwise."
  (interactive)
  (unless doom-popup-history
    (error "No popups to restore"))
  (let (any-p)
    (dolist (spec doom-popup-history)
      (let ((buffer (get-buffer (car spec)))
            (file   (plist-get (cdr spec) :file))
            (rules  (plist-get (cdr spec) :rules))
            (size   (plist-get (cdr spec) :size)))
        (when (and (not buffer) file)
          (setq buffer
                (if-let (buf (get-file-buffer file))
                    (clone-indirect-buffer (buffer-name buf) nil t)
                  (find-file-noselect file t))))
        (when size
          (setq rules (plist-put rules :size size)))
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
  (when-let (popups (doom-popup-windows))
    (let (success doom-popup-remember-history)
      (setq doom-popup-history (mapcar #'doom--popup-data popups))
      (dolist (window popups)
        (when (or force-p
                  (called-interactively-p 'interactive)
                  (doom-popup-prop :autoclose window))
          (delete-window window)
          (setq success t)))
      success)))

;;;###autoload
(defun doom/popup-close-maybe ()
  "Close the current popup *if* its window doesn't have a noesc parameter."
  (interactive)
  (if (doom-popup-prop :noesc)
      (call-interactively
       (if (featurep 'evil)
           #'evil-force-normal-state
         #'keyboard-escape-quit))
    (delete-window)))

;;;###autoload
(defun doom/popup-this-buffer ()
  "Display currently selected buffer in a popup window."
  (interactive)
  (doom-popup-buffer (current-buffer) :align t :autokill t))

;;;###autoload
(defun doom/popup-toggle-messages ()
  "Toggle *Messages* buffer."
  (interactive)
  (if-let (win (get-buffer-window "*Messages*"))
      (doom/popup-close win)
    (doom-popup-buffer (get-buffer "*Messages*"))))

;;;###autoload
(defun doom-popup-prop (prop &optional window)
  "Returns a `doom-popup-rules' PROPerty from WINDOW."
  (or (plist-get (or (if window
                         (ignore-errors
                           (buffer-local-value 'doom-popup-rules
                                               (window-buffer window)))
                       doom-popup-rules)
                     (window-parameter window 'popup))
                 prop)
      (pcase prop
        (:size  shackle-default-size)
        (:align shackle-default-alignment))))

;;;###autoload
(defun doom-popup-side (&optional window)
  "Return what side a popup WINDOW came from ('left 'right 'above or 'below)."
  (let ((align (doom-popup-prop :align window)))
    (when (eq align t)
      (setq align shackle-default-alignment))
    (when (functionp align)
      (setq align (funcall align)))
    align))

;;;###autoload
(defun doom-popup-size (&optional window)
  "Return the size of a popup WINDOW."
  (pcase (doom-popup-side window)
    ((or 'left 'right)  (window-width window))
    ((or 'above 'below) (window-height window))))

(defun doom--popup-data (window)
  (when-let (buffer (window-buffer window))
    `(,(buffer-name buffer)
      :file  ,(buffer-file-name buffer)
      :rules ,(window-parameter window 'popup)
      :size  ,(doom-popup-size window))))

;;;###autoload
(defmacro with-popup-rules! (rules &rest body)
  "TODO"
  (declare (indent defun))
  `(let ((old-shackle-rules shackle-rules))
     ,@(cl-loop for rule in rules
                collect `(set! :popup ,@rule))
     ,@body
     (setq shackle-rules old-shackle-rules)))

;;;###autoload
(defun doom/other-popup (count)
  "Cycle through popup windows. Like `other-window', but for popups."
  (interactive "p")
  (if-let (popups (if (doom-popup-p)
                      (cdr (memq (selected-window) doom-popup-windows))
                    (setq doom-popup-other-window (selected-window))
                    doom-popup-windows))
      (select-window (nth (mod (1- count) (length popups)) popups))
    (unless (eq (selected-window) doom-popup-other-window)
      (when doom-popup-other-window
        (select-window doom-popup-other-window t)
        (cl-decf count))
      (when (/= count 0)
        (other-window count)))))
