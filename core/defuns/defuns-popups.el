;;; defuns-popups.el

;;;###autoload
(defun doom/popup-remove (window)
  (setq doom-popup-windows (delete window doom-popup-windows)))

;;;###autoload
(defun doom/popup-p (&optional window)
  "Whether WINDOW is a shackle popup window or not."
  (and doom-popup-windows
       (-any? (lambda (w)
                (if (window-live-p w) t (doom/popup-remove w) nil))
              doom-popup-windows)
       (if window
           (-any? (lambda (w) (eq window w)) doom-popup-windows)
         t)))

;;;###autoload
(defmacro doom/popup-save (&rest body)
  `(let ((popup-p (doom/popup-p)))
     (when popup-p (doom/popup-close-all t))
     ,@body
     (when popup-p
       (save-selected-window
         (doom/popup-last-buffer)))))

;;;###autoload
(defun doom/popup-buffer (buffer &optional plist)
  "Display BUFFER in a shackle popup."
  (let ((buffer-name (if (stringp buffer) buffer (buffer-name buffer))))
    (shackle-display-buffer (get-buffer-create buffer-name)
                            nil (or plist (shackle-match buffer-name)))))

;;;###autoload
(defun doom/popup-close (&optional window dont-kill dont-close-all)
  "Find and close the currently active popup (if available)."
  (interactive)
  (when (not window)
    (if (doom/popup-p (selected-window))
        (setq window (selected-window))
      (unless dont-close-all
        (doom/popup-close-all dont-kill))))
  (when (and window (window-live-p window))
    ;; REPL buffer
    (cond ((and (derived-mode-p 'comint-mode)
                (featurep 'repl-toggle)
                repl-toggle-mode)
           (setq rtog/--last-buffer nil))
          ((eq major-mode 'messages-buffer-mode)
           (bury-buffer)
           (setq dont-kill t)))
    (doom/popup-remove window)
    (unless dont-kill
      (let ((kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions)))
        (kill-buffer (window-buffer window))))
    (delete-window window)))

;;;###autoload
(defun doom/popup-close-all (&optional dont-kill-buffers)
  "Closes all popup windows (and kills the buffers if DONT-KILL-BUFFERS is non-nil)"
  (interactive)
  (mapc (lambda (w) (doom/popup-close w dont-kill-buffers))
        doom-popup-windows)
  (setq doom-popup-windows nil))

;;;###autoload
(defun doom/popup-toggle ()
  "Toggles the popup window, reopening the last popup (if available)."
  (interactive)
  (if (doom/popup-p)
      (doom/popup-close t)
    (doom/popup-last-buffer)))

;;;###autoload
(defun doom/popup-last-buffer ()
  "Pop up the last popup buffer."
  (interactive)
  (unless shackle-last-buffer
    (error "No popup to restore"))
  (doom/popup-buffer shackle-last-buffer))

;;;###autoload
(defun doom/popup-messages ()
    "Pop up the *Messages* buffer."
    (interactive)
    (doom/popup-buffer "*Messages*")
    (with-current-buffer "*Messages*"
      (doom|hide-mode-line)
      (goto-char (point-max))))

;;;###autoload
(defun doom|popup-init ()
  (add-to-list 'doom-popup-windows (get-buffer-window))
  (local-set-key [escape escape] 'doom/popup-close)
  (when (or (bound-and-true-p repl-toggle-mode)
            (derived-mode-p 'tabulated-list-mode)
            (memq major-mode '(messages-buffer-mode flycheck-error-list-mode-hook esup-mode)))
    (let ((map evil-normal-state-local-map))
      (define-key map [escape] 'doom/popup-close)
      (define-key map (kbd "ESC") 'doom/popup-close))))

(defvar shackle-popup-hook '() "Hook run whenever a popup is opened.")
;;;###autoload
(defun doom|run-popup-hooks (&rest _)
  (with-current-buffer shackle-last-buffer
    (run-hooks 'shackle-popup-hook)))

(provide 'defuns-popups)
;;; defuns-popups.el ends here
