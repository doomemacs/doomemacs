;;; core-quickrun.el

(use-package quickrun
  :commands (quickrun
             quickrun-region
             quickrun-with-arg
             quickrun-shell
             quickrun-compile-only
             quickrun-replace-region
             helm-quickrun)
  :config
  (setq quickrun-focus-p t)
  (add-hook! quickrun/mode '(linum-mode yascroll-bar-mode))
  (add-to-list 'quickrun-file-alist '("\\.gvy$" . "groovy")))

(use-package repl-toggle
  :commands (rtog/toggle-repl rtog/add-repl)
  :init
  (setq rtog/goto-buffer-fun 'popwin:pop-to-buffer
        rtog/mode-repl-alist '())

  (add-hook! repl-toggle-mode 'yascroll-bar-mode)
  (add-hook! repl-toggle-mode (evil-initialize-state 'emacs)))

(provide 'core-quickrun)
;;; core-quickrun.el ends here
