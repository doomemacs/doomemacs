;;; custom-debug.el --- debugging with `realgud'

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

  ;; Popup rules
  (def-popup! "\\`\\*\\(g\\|zsh\\|bash\\)db.*?\\*\\'" :size 20 :regexp t)
  (def-popup! "\\`\\*trepanjs.*?\\*\\'"               :size 20 :regexp t)

  ;; Temporary Ex commands for the debugger
  (def-tmp-excmd! doom:def-debug-on doom:def-debug-off
    ("n[ext]" . realgud:cmd-next)
    ("s[tep]" . realgud:cmd-step)
    ("b[reak]" . doom:debug-toggle-breakpoint)
    ("c[ontinue]" . realgud:cmd-continue))

  ;; TODO does this work with shackle?
  (advice-add 'realgud-cmdbuf-init :after 'doom:def-debug-on)
  (advice-add 'realgud:cmd-quit :after 'doom:def-debug-off)
  ;; Monkey-patch `realgud:run-process' to run in a popup.
  (advice-add 'realgud:run-process :override 'doom*realgud:run-process))

(provide 'custom-debug)
;;; custom-debug.el ends here
