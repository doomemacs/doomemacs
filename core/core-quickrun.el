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
  (add-to-list 'quickrun-file-alist '("\\.gvy$" . "groovy")))

(provide 'core-quickrun)
;;; core-quickrun.el ends here
