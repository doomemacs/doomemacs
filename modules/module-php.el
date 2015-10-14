;;; module-php.el

(use-package php-mode
  :mode "\\.\\(php\\|inc\\)$"
  :init
  (add-hook! php-mode 'flycheck-mode)
  (setq php-template-compatibility nil
        php-extras-eldoc-functions-file (concat narf-temp-dir "php-extras-eldoc-functions"))
  :config
  (require 'php-extras)
  (define-company-backend! php-mode '(php-extras-company))

  ;; TODO Tie into emr
  (require 'php-refactor-mode)
  (add-hook! php-mode '(turn-on-eldoc-mode emr-initialize php-refactor-mode)))

(use-package hack-mode :mode "\\.hh$")

(provide 'module-php)
;;; module-php.el ends here
