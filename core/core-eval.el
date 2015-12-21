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
  (setq quickrun-focus-p nil)
  (add-hook! quickrun/mode '(linum-mode))
  (add-to-list 'quickrun-file-alist '("\\.gvy$" . "groovy")))

(use-package repl-toggle
  :commands (rtog/toggle-repl rtog/add-repl)
  :init
  (setq rtog/mode-repl-alist '())

  (defvar repl-p nil)
  (make-variable-buffer-local 'repl-p)

  (defun narf|repl-init ()
    (yascroll-bar-mode +1)
    (evil-initialize-state 'emacs)
    (setq mode-line-format nil
          repl-p t))
  (add-hook! repl-toggle-mode 'narf|repl-init))

(provide 'core-eval)
;;; core-eval.el ends here
