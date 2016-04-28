;;; defuns-popups.el

;;;###autoload
(defun narf/popup-remove (window)
  (setq narf-popup-windows (delete window narf-popup-windows)))

;;;###autoload
(defun narf/popup-p (&optional window)
  "Whether WINDOW is a shackle popup window or not."
  (and narf-popup-windows
       (-any? (lambda (w)
                (if (window-live-p w) t (narf/popup-remove w) nil))
              narf-popup-windows)
       (if window
           (-any? (lambda (w) (eq window w)) narf-popup-windows)
         t)))

;;;###autoload
(defmacro narf/popup-save (&rest body)
  `(let ((popup-p (narf/popup-p)))
     (when popup-p (narf/popup-close-all t))
     ,@body
     (when popup-p
       (save-selected-window
         (narf/popup-last-buffer)))))

;;;###autoload
(defun narf/popup-buffer (buffer &optional plist)
  "Display BUFFER in a shackle popup."
  (let ((buffer-name (if (stringp buffer) buffer (buffer-name buffer))))
    (shackle-display-buffer (get-buffer-create buffer-name)
                            nil (or plist (shackle-match buffer-name)))))

;;;###autoload
(defun narf/popup-close (&optional window dont-kill dont-close-all)
  "Find and close the currently active popup (if available)."
  (interactive)
  (when (not window)
    (if (narf/popup-p (selected-window))
        (setq window (selected-window))
      (unless dont-close-all
        (narf/popup-close-all dont-kill))))
  (when (and window (window-live-p window))
    ;; REPL buffer
    (cond ((and (derived-mode-p 'comint-mode)
                (featurep 'repl-toggle)
                repl-toggle-mode)
           (setq rtog/--last-buffer nil))
          ((eq major-mode 'messages-buffer-mode)
           (bury-buffer)
           (setq dont-kill t)))
    (narf/popup-remove window)
    (unless dont-kill
      (let ((kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions)))
        (kill-buffer (window-buffer window))))
    (delete-window window)))

;;;###autoload
(defun narf/popup-close-all (&optional dont-kill-buffers)
  "Closes all popup windows (and kills the buffers if DONT-KILL-BUFFERS is non-nil)"
  (interactive)
  (mapc (lambda (w) (narf/popup-close w dont-kill-buffers))
        narf-popup-windows)
  (setq narf-popup-windows nil))

;;;###autoload
(defun narf/popup-toggle ()
  "Toggles the popup window, reopening the last popup (if available)."
  (interactive)
  (if (narf/popup-p)
      (narf/popup-close t)
    (narf/popup-last-buffer)))

;;;###autoload
(defun narf/popup-last-buffer ()
  "Pop up the last popup buffer."
  (interactive)
  (unless shackle-last-buffer
    (error "No popup to restore"))
  (narf/popup-buffer shackle-last-buffer))

;;;###autoload
(defun narf/popup-messages ()
    "Pop up the *Messages* buffer."
    (interactive)
    (narf/popup-buffer "*Messages*")
    (with-current-buffer "*Messages*"
      (narf|hide-mode-line)
      (goto-char (point-max))))

;;;###autoload
(defun narf|popup-init ()
  (add-to-list 'narf-popup-windows (get-buffer-window))
  (local-set-key [escape escape] 'narf/popup-close)
  (when (or (bound-and-true-p repl-toggle-mode)
            (derived-mode-p 'tabulated-list-mode)
            (memq major-mode '(messages-buffer-mode flycheck-error-list-mode-hook esup-mode)))
    (let ((map evil-normal-state-local-map))
      (define-key map [escape] 'narf/popup-close)
      (define-key map (kbd "ESC") 'narf/popup-close))))

(defvar shackle-popup-hook '() "Hook run whenever a popup is opened.")
;;;###autoload
(defun narf|run-popup-hooks (&rest _)
  (with-current-buffer shackle-last-buffer
    (run-hooks 'shackle-popup-hook)))

(provide 'defuns-popups)
;;; defuns-popups.el ends here
