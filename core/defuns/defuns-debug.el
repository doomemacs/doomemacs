;;; defuns-debug.el

;;;###autoload
(defun what-face (pos)
  "Tells you the name of the face (point) is on."
  (interactive "d")
  (let ((hl-line-p hl-line-mode))
    (if hl-line-p (hl-line-mode -1))
    (let ((face (or (get-char-property (point) 'read-face-name)
                    (get-char-property (point) 'face))))
      (if face (message "Face: %s" face) (message "No face at %d" pos)))
    (if hl-line-p (hl-line-mode 1))))

;;;###autoload
(defun what-col ()
  (interactive)
  (message "Column %d" (current-column)))

;;;###autoload
(defun what-bindings (key)
  (list
   (minor-mode-key-binding key)
   (local-key-binding key)
   (global-key-binding key)))

;;;###autoload
(defun what-major-mode ()
  (interactive)
  (message "Mode: %s" major-mode))

;;;###autoload (autoload 'narf:echo "defuns-debug" nil t)
(evil-define-command narf:echo (bang message)
  "Display MSG in echo-area without logging it in *Messages* buffer."
  (interactive "<!><a>")
  (let (message-log-max)
    (message "%s%s" (if bang ">> " "") message)))

;;;###autoload (autoload 'narf:debug "defuns-debug" nil t)
(evil-define-command narf:debug (&optional path)
  "Initiate debugger for current major mode"
  (interactive "<f>")
  (let ((default-directory (narf/project-root)))
    (cond ((memq major-mode '(c-mode c++-mode))
           (realgud:gdb (if path (concat "gdb " path))))
          ((memq major-mode '(ruby-mode enh-ruby-mode))
           (narf:repl nil (format "run '%s'" (f-filename (or path buffer-file-name)))))
          ((eq major-mode 'sh-mode)
           (let ((shell sh-shell))
             (when (string= shell "sh")
               (setq shell "bash"))
             (cond ((string= shell "bash")
                    (realgud:bashdb (if path (concat "bashdb " path))))
                   ((string= shell "zsh")
                    (realgud:zshdb (if path (concat "zshdb " path))))
                   (t (user-error "No shell debugger for %s" shell)))))
          ;; TODO Add python debugging
          ((memq major-mode '(js-mode js2-mode js3-mode))
           (realgud:trepanjs))
          (t (user-error "No debugger for %s" major-mode)))))

;;;###autoload (autoload 'narf:debug-toggle-breakpoint "defuns-debug" nil t)
(evil-define-command narf:debug-toggle-breakpoint (&optional bang)
  (interactive "<!>")
  (call-interactively (if bang 'realgud:cmd-clear 'realgud:cmd-break)))

;;;###autoload
(defun narf/debug-quit ()
  (interactive)
  (ignore-errors (call-interactively 'realgud:cmd-quit))
  (narf/popup-close)
  (evil-normal-state))

(provide 'defuns-debug)
;;; defuns-debug.el ends here
