;;; module-php.el

(use-package php-mode
  :mode "\\.\\(php\\|inc\\)$"
  :init
  (setq php-template-compatibility nil
        php-extras-eldoc-functions-file (concat narf-temp-dir "php-extras-eldoc-functions"))
  :config
  (require 'php-extras)
  (add-company-backend! php-mode '(php-extras-company))

  ;; TODO Tie into emr
  (require 'php-refactor-mode)
  (add-hook! php-mode '(turn-on-eldoc-mode emr-initialize php-refactor-mode)))

(provide 'module-php)
;;; module-php.el ends here
