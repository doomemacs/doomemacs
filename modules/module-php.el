;;; module-php.el

(use-package hack-mode
  :mode "\\.hh$"
  :config (define-company-backend! hack-mode (capf)))

(use-package php-mode
  :mode ("\\.php[s345]?$" "\\.inc$" )
  :interpreter "php"
  :init
  (define-docset! php-mode "php,laravel")
  (define-company-backend! php-mode '(php-extras-company))

  (add-hook 'php-mode-hook 'flycheck-mode)
  (setq php-template-compatibility nil
        php-extras-eldoc-functions-file (concat narf-temp-dir "/php-extras-eldoc-functions"))
  :config
  (require 'php-extras)
  (defun php-extras-company-setup ()) ;; company will set up itself

  (map! :map php-mode-map
        (:localleader :nv ";" 'narf/append-semicolon)
        :n "gd" 'ac-php-find-symbol-at-point
        :n "gD" 'ac-php-location-stack-back)

  ;; Generate php-extras documentation and completion asynchronously
  (unless (file-exists-p (concat php-extras-eldoc-functions-file ".el"))
    (async-start `(lambda ()
                    ,(async-inject-variables "\\`\\(load-path\\|php-extras-eldoc-functions-file\\)$")
                    (require 'php-extras-gen-eldoc)
                    (php-extras-generate-eldoc-1 t))
                 (lambda (_)
                   (load (concat php-extras-eldoc-functions-file ".el"))
                   (message "PHP eldoc updated!"))))

  (sp-with-modes '(php-mode)
    (sp-local-pair "/*"    "*/"   :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
    (sp-local-pair "/**"   "*/"   :post-handlers '(("||\n[i]" "RET") ("||\n[i]" "SPC")))
    (sp-local-pair "<? "    " ?>" :post-handlers '(("||\n[i]" "RET") ("| " "SPC") ))
    (sp-local-pair "<?php " " ?>" :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
    (sp-local-pair "<?="    " ?>")
    (sp-local-pair "<?"    "?>"   :when '(("RET")) :post-handlers '("||\n[i]"))
    (sp-local-pair "<?php" "?>"   :when '(("RET")) :post-handlers '("||\n[i]")))

  (use-package php-refactor-mode
    :init (add-hook! php-mode '(turn-on-eldoc-mode emr-initialize php-refactor-mode))
    :config
    (require 'emr)
    (mapc (lambda (x)
            (let ((command-name (car x))
                  (title (cadr x))
                  (region-p (caddr x))
                  predicate)
              (setq predicate (cond ((eq region-p 'both) nil)
                                    (t (if region-p
                                           (lambda () (use-region-p))
                                         (lambda () (not (use-region-p)))))))
              (emr-declare-command (intern (format "php-refactor--%s" (symbol-name command-name)))
                :title title :modes 'php-mode :predicate predicate)))
          '((convert-local-to-instance-variable "convert local var to instance var" nil)
            (optimize-use                       "optimize FQNs in file"   nil)
            (extract-method                     "extract method"          t)
            (rename-local-variable              "rename local variable"   nil)))))

;; PHP Repl
(use-package php-boris :defer t
  :init (define-repl! php-mode php-boris)
  :config (evil-set-initial-state 'php-boris-mode 'emacs))

;;
(define-project-type! laravel "laravel"
  :modes (php-mode json-mode yaml-mode web-mode nxml-mode js2-mode scss-mode)
  :files ("artisan" "server.php"))

(provide 'module-php)
;;; module-php.el ends here
