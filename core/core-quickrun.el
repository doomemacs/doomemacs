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
  (add-hook! quickrun/mode 'linum-mode)
  (add-hook! quickrun/mode 'yascroll-bar-mode)

  (add-to-list 'quickrun-file-alist '("\\.gvy$" . "groovy")))

(use-package repl-toggle
  :commands (rtog/toggle-repl rtog/add-repl)
  :config
  (setq rtog/goto-buffer-fun 'popwin:pop-to-buffer
        rtog/mode-repl-alist
        '((php-mode . php-boris)
          (python-mode . run-python)
          (lua-mode . run-lua)
          (emacs-lisp-mode . ielm)
          (ruby-mode . inf-ruby)
          (js2-mode . nodejs-repl)))
  )

(provide 'core-quickrun)
;;; core-quickrun.el ends here
