;;; core-eval.el

;; + Running inline code + REPLs (using `quickrun' + `repl-toggle')
;; + Almost-universal debugging (with `realgud')
;; + Simple code navigation (using `dump-jump' and `imenu-list')
;; + A universal tags config (WIP)

;; remove ellipsis when printing sexp in message buffer
(setq eval-expression-print-length nil
      eval-expression-print-level  nil)

(use-package quickrun
  :commands (quickrun
             quickrun-region
             quickrun-with-arg
             quickrun-shell
             quickrun-compile-only
             quickrun-replace-region
             helm-quickrun)
  :init (add-hook 'quickrun/mode-hook 'linum-mode)
  :config (setq quickrun-focus-p nil))

(use-package repl-toggle
  :commands (rtog/toggle-repl rtog/add-repl)
  :init
  (defvar doom--repl-buffer nil "The current REPL buffer.")
  (defvar-local repl-p nil "Whether this is a repl buffer or not.")
  (setq rtog/mode-repl-alist '())
  (add-hook! repl-toggle-mode (evil-initialize-state 'emacs))

  :config
  (map! :map repl-toggle-mode-map
        :ei "C-n" 'comint-next-input
        :ei "C-p" 'comint-previous-input
        :ei "<down>" 'comint-next-input
        :ei "<up>"   'comint-previous-input))

;;
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
  (def-tmp-excmd! doom:def-debug-on doom:def-debug-off
    ("n[ext]" . realgud:cmd-next)
    ("s[tep]" . realgud:cmd-step)
    ("b[reak]" . doom:debug-toggle-breakpoint)
    ("c[ontinue]" . realgud:cmd-continue))

  ;; TODO does this work with shackle?
  (advice-add 'realgud-cmdbuf-init :after 'doom:def-debug-on)
  (advice-add 'realgud:cmd-quit :after 'doom:def-debug-off))

(provide 'core-eval)
;;; core-eval.el ends here
