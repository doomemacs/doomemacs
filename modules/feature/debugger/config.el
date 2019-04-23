;;; feature/debugger/config.el -*- lexical-binding: t; -*-

(def-package! realgud
  :commands (realgud:gdb realgud:trepanjs realgud:bashdb realgud:zshdb)
  :config
  (set-popup-rule! "^\\*\\(?:trepanjs:\\(?:g\\|zsh\\|bash\\)db\\)" :size 20)

  ;; TODO Temporary Ex commands for the debugger
  ;; (def-tmp-excmd! doom:def-debug-on doom:def-debug-off
  ;;   ("n[ext]" . realgud:cmd-next)
  ;;   ("s[tep]" . realgud:cmd-step)
  ;;   ("b[reak]" . +debug:toggle-breakpoint)
  ;;   ("c[ontinue]" . realgud:cmd-continue))
  ;; (advice-add #'realgud-cmdbuf-init :after #'doom:def-debug-on)
  ;; (advice-add #'realgud:cmd-quit :after #'doom:def-debug-off)

  ;; Monkey-patch `realgud:run-process' to run in a popup.
  ;; TODO Find a more elegant solution
  ;; FIXME Causes realgud:cmd-* to focus popup on every invocation
  (defun +debugger*realgud-run-process
      (debugger-name script-filename cmd-args minibuffer-history-var &optional no-reset)
    (let* ((cmd-buf (apply #'realgud-exec-shell debugger-name script-filename
                           (car cmd-args) no-reset (cdr cmd-args)))
           (process (get-buffer-process cmd-buf)))
      (cond ((and process (eq 'run (process-status process)))
             (pop-to-buffer cmd-buf)
             (define-key evil-emacs-state-local-map (kbd "ESC ESC") #'+debug/quit)
             (realgud:track-set-debugger debugger-name)
             (realgud-cmdbuf-info-in-debugger?= 't)
             (realgud-cmdbuf-info-cmd-args= cmd-args)
             (when cmd-buf
               (switch-to-buffer cmd-buf)
               (when realgud-cmdbuf-info
                 (let* ((info realgud-cmdbuf-info)
                        (cmd-args (realgud-cmdbuf-info-cmd-args info))
                        (cmd-str  (mapconcat #'identity cmd-args " ")))
                   (set minibuffer-history-var
                        (list-utils-uniq (cons cmd-str (eval minibuffer-history-var))))))))
            (t
             (if cmd-buf (switch-to-buffer cmd-buf))
             (message "Error running command: %s" (mapconcat #'identity cmd-args " "))))
      cmd-buf))
  (advice-add #'realgud:run-process :override #'+debugger*realgud-run-process))

