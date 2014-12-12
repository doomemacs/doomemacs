(use-package php-mode
  :mode "\\.\\(php\\|inc\\)$"
  :config
  (progn
    (add-hook! 'php-mode-hook (setq my-run-code-interpreter "php"))
    (setq php-template-compatibility nil)))


(provide 'init-php)
;;; init-php.el ends here
