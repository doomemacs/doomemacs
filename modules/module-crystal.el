;;; module-crystal.el

(use-package crystal-mode
  :mode "\\.cr$"
  :interpreter "crystal"
  :config
  (setq crystal-indent-level 2))

(provide 'module-crystal)
;;; module-crystal.el ends here
