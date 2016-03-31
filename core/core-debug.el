;;; core-debug.el

(after! debug
  ;; For elisp debugging
  (map! :map debugger-mode-map
        :n "RET" 'debug-help-follow
        :n "n" 'debugger-step-through
        :n "c" 'debugger-continue))

(use-package realgud
  :commands (realgud:gdb realgud:trepanjs realgud:bashdb realgud:zshdb)
  :config
  (map! :map realgud:shortkey-mode-map
        :n "j" 'evil-next-line
        :n "k" 'evil-previous-line
        :n "h" 'evil-backward-char
        :n "l" 'evil-forward-char
        ;; FIXME Greedy command buffer always grabs focus
        :m "n" 'realgud:cmd-next
        :m "b" 'realgud:cmd-break
        :m "B" 'realgud:cmd-clear
        :n "c" 'realgud:cmd-continue)

  ;; Temporary Ex commands for the debugger
  (define-temp-ex-cmd! narf:def-debug-on narf:def-debug-off
    ("n[ext]" . realgud:cmd-next)
    ("s[tep]" . realgud:cmd-step)
    ("b[reak]" . narf:debug-toggle-breakpoint)
    ("c[ontinue]" . realgud:cmd-continue))

  (advice-add 'realgud-cmdbuf-init :after 'narf:def-debug-on)
  (advice-add 'realgud:cmd-quit :after 'narf:def-debug-off))

(provide 'core-debug)
;;; core-debug.el ends here
