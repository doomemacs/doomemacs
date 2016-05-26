;;; defuns-popups.el

;;;###autoload
(defun doom/popup-remove (window)
  (setq doom-popup-windows (delete window doom-popup-windows))
  (unless (and doom-prev-buffer (not (buffer-live-p doom-prev-buffer)))
    (setq doom-prev-buffer nil)))

;;;###autoload
(defun doom/popup-p (&optional window)
  "Whether WINDOW is a shackle popup window or not."
  (and doom-popup-windows
       (--any? (if (window-live-p it) t (doom/popup-remove it) nil)
               doom-popup-windows)
       (if window
           (--any? (eq window it) doom-popup-windows)
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
  (let* ((buffer-name (if (stringp buffer) buffer (buffer-name buffer)))
         (buffer (get-buffer-create buffer-name)))
    (shackle-display-buffer buffer nil (or plist (shackle-match buffer-name)))
    (setq doom-last-popup buffer)))

;;;###autoload
(defun doom/popup-close (&optional window dont-kill dont-close-all)
  "Find and close the currently active popup (if available)."
  (interactive)
  (let ((dont-kill (or doom-popup-protect dont-kill)))
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
        (let ((kill-buffer-query-functions
               (delq 'process-kill-buffer-query-function
                     kill-buffer-query-functions)))
          (kill-buffer (window-buffer window))))
      (delete-window window))))

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
  (unless (and doom-last-popup
               (buffer-live-p doom-last-popup))
    (setq doom-last-popup nil)
    (error "No popup to restore"))
  (doom/popup-buffer doom-last-popup))

;;;###autoload
(defun doom/popup-messages ()
  "Pop up the *Messages* buffer."
  (interactive)
  (doom/popup-buffer "*Messages*")
  (with-current-buffer "*Messages*"
    (doom|hide-mode-line)
    (goto-char (point-max))))

;;;###autoload
(defun doom*run-popup-pre-hooks (&rest _)
  (unless (and doom-prev-buffer
               (doom/popup-p (get-buffer-window doom-prev-buffer)))
    (setq doom-prev-buffer (current-buffer))))

;;;###autoload
(defun doom*run-popup-post-hooks (&rest _)
  (with-current-buffer shackle-last-buffer
    (run-hooks 'doom-popup-post-hook)))

;;;###autoload
(defun doom|popup-init ()
  (add-to-list 'doom-popup-windows (get-buffer-window))
  (unless (one-window-p)
    (ignore-errors
      (local-set-key [escape escape] 'doom/popup-close)
      (let ((map evil-normal-state-local-map))
        (define-key map [escape escape] 'doom/popup-close)
        (unless (or (apply #'derived-mode-p doom-popup-inescapable-modes)
                    (--any? (string-match-p it buffer-name) (-filter 'stringp doom-popup-inescapable-modes)))
          (define-key map [escape] 'doom/popup-close)
          (define-key map (kbd "ESC") 'doom/popup-close)))
      (when (or (apply #'derived-mode-p doom-popup-protect-modes)
                (--any? (string-match-p it buffer-name) (-filter 'stringp doom-popup-protect-modes)))
        (setq-local doom-popup-protect t)
        (setq doom-last-popup (current-buffer))))))

;;;###autoload
(defun doom*save-popup (orig-fun &rest args)
  "Prevents messing up a popup buffer on window changes"
  (doom/popup-save (apply orig-fun args)))

(provide 'defuns-popups)
;;; defuns-popups.el ends here
