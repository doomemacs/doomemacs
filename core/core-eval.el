;;; core-eval.el

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
  (setq rtog/mode-repl-alist '())

  (defun narf|repl-init ()
    (yascroll-bar-mode +1)
    (evil-initialize-state 'emacs)
    (setq mode-line-format nil))
  (add-hook! repl-toggle-mode 'narf|repl-init)
  :config
  (map! :map repl-toggle-mode-map
        "ESC ESC" 'narf/popup-close))

(provide 'core-eval)
;;; core-eval.el ends here
