;;; defuns-realgud.el

;; TODO Find a more elegant solution
;; FIXME Causes realgud:cmd-* to focus popup on every invocation
;;;###autoload
(defun doom*realgud:run-process (debugger-name script-filename cmd-args minibuffer-history &optional no-reset)
  (let ((cmd-buf))
    (setq cmd-buf
          (apply 'realgud-exec-shell debugger-name script-filename
                 (car cmd-args) no-reset (cdr cmd-args)))
    (let ((process (get-buffer-process cmd-buf)))
      (if (and process (eq 'run (process-status process)))
          (progn
            (pop-to-buffer cmd-buf)
            (define-key evil-emacs-state-local-map (kbd "ESC ESC") 'doom/debug-quit)
            (realgud:track-set-debugger debugger-name)
            (realgud-cmdbuf-info-in-debugger?= 't)
            (realgud-cmdbuf-info-cmd-args= cmd-args)
            (when cmd-buf
              (switch-to-buffer cmd-buf)
              (when realgud-cmdbuf-info
                (let* ((info realgud-cmdbuf-info)
                       (cmd-args (realgud-cmdbuf-info-cmd-args info))
                       (cmd-str  (mapconcat 'identity  cmd-args " ")))
                  (set minibuffer-history
                       (list-utils-uniq (cons cmd-str (eval minibuffer-history))))))))
        ;; else
        (progn
          (if cmd-buf (switch-to-buffer cmd-buf))
          (message "Error running command: %s" (mapconcat 'identity cmd-args " ")))))
    cmd-buf))


;;;###autoload (autoload 'doom:debug-toggle-breakpoint "defuns-realgud" nil t)
(evil-define-command doom:debug-toggle-breakpoint (&optional bang)
  (interactive "<!>")
  (call-interactively (if bang 'realgud:cmd-clear 'realgud:cmd-break)))

;;;###autoload
(defun doom/debug-quit ()
  (interactive)
  (ignore-errors (call-interactively 'realgud:cmd-quit))
  (doom/popup-close)
  (evil-normal-state))

;;;###autoload (autoload 'doom:debug "defuns-realgud" nil t)
(evil-define-command doom:debug (&optional path)
  "Initiate debugger for current major mode"
  (interactive "<f>")
  (let ((default-directory (doom/project-root)))
    (cond ((memq major-mode '(c-mode c++-mode))
           (realgud:gdb (if path (concat "gdb " path))))
          ((memq major-mode '(ruby-mode enh-ruby-mode))
           (doom:repl nil (format "run '%s'" (f-filename (or path buffer-file-name)))))
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
          ((eq major-mode 'haskell-mode)
           (haskell-debug))
          (t (user-error "No debugger for %s" major-mode)))))


(provide 'defuns-realgud)
;;; defuns-realgud.el ends here
