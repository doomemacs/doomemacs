(use-package php-mode
  :mode "\\.\\(php\\|inc\\)$"
  :config
  (progn
    (setq php-template-compatibility nil)
    (add-hook 'php-mode-hook 'turn-on-eldoc-mode)

    (use-package php-extras
      :config
      (narf/add-company-backend php-mode (php-extras-company)))

    ;; TODO Tie into emr
    (use-package php-refactor-mode
      :config
      (progn
        (add-hook 'php-mode-hook 'emr-initialize)
        (add-hook 'php-mode-hook 'php-refactor-mode)))))


(provide 'init-php)
;;; init-php.el ends here
