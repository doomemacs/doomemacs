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
        :m "n" 'realgud:cmd-next
        :m "b" 'realgud:cmd-break
        :m "B" 'realgud:cmd-clear
        :n "c" 'realgud:cmd-continue)

  ;; Temporary Ex commands for the debugger
  ;; TODO Turn temporary ex commands into macro
  (defun narf*debug-init (&rest _)
    (exmap "n[ext]"   'realgud:cmd-next)
    (exmap "s[tep]"   'realgud:cmd-step)
    (exmap "b[reak]"  'narf:debug-toggle-breakpoint)
    (exmap "c[ontinue]" 'realgud:cmd-continue))
  (defun narf*debug-quit (&rest _)
    (narf/evil-ex-undefine-cmd "n[ext]")
    (narf/evil-ex-undefine-cmd "s[tep]")
    (narf/evil-ex-undefine-cmd "b[reak]")
    (narf/evil-ex-undefine-cmd "c[ontinue]"))
  (advice-add 'realgud-cmdbuf-init :after 'narf*debug-init)
  (advice-add 'realgud:cmd-quit :after 'narf*debug-quit))

(provide 'core-debug)
;;; core-debug.el ends here
