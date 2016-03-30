;;; core-eval.el

(use-package quickrun
  :commands (quickrun
             quickrun-region
             quickrun-with-arg
             quickrun-shell
             quickrun-compile-only
             quickrun-replace-region
             helm-quickrun)
  :init
  (add-hook 'quickrun/mode-hook 'linum-mode)
  :config
  (setq quickrun-focus-p nil)
  (push '("\\.gvy$" . "groovy") quickrun-file-alist))

(use-package repl-toggle
  :commands (rtog/toggle-repl rtog/add-repl)
  :init
  (setq rtog/mode-repl-alist '())

  (defvar repl-p nil)
  (make-variable-buffer-local 'repl-p)

  (add-hook! repl-toggle-mode
    (evil-initialize-state 'emacs)
    (narf|hide-mode-line)
    (setq repl-p t))

  :config
  (map! :map repl-toggle-mode-map
        :ei "C-n" 'comint-next-input
        :ei "C-p" 'comint-previous-input
        :ei "<down>" 'comint-next-input
        :ei "<up>"   'comint-previous-input))

(provide 'core-eval)
;;; core-eval.el ends here
