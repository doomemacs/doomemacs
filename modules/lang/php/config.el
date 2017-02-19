;;; lang/php/config.el

;; (@def-package hack-mode
;;   :mode "\\.hh$"
;;   :config
;;   (@set :company-backend 'hack-mode '(company-capf)))


(@def-package php-mode
  :mode ("\\.php[s345]?$" "\\.inc$")
  :interpreter "php"
  :init
  (add-hook 'php-mode-hook 'flycheck-mode)
  :config
  (setq php-template-compatibility nil)

  (@set :repl 'php-mode 'php-boris)
  (@set :company-bakend 'php-mode '(php-extras-company company-yasnippet))

  (@add-hook php-mode (setq-local sp-max-pair-length 6))

  (sp-with-modes '(php-mode)
    (sp-local-pair "/*"    "*/" :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
    (sp-local-pair "/**"   "*/" :post-handlers '(("||\n[i]" "RET") ("||\n[i]" "SPC")))
    (sp-local-pair "<? "    " ?>")
    (sp-local-pair "<?php " " ?>")
    (sp-local-pair "<?="    " ?>")
    (sp-local-pair "<?"    "?>"   :when '(("RET")) :post-handlers '("||\n[i]"))
    (sp-local-pair "<?php" "?>"   :when '(("RET")) :post-handlers '("||\n[i]")))

  (@map :map php-mode-map
        :localleader
        :nv ";" 'doom/append-semicolon

        :prefix "r"
        :n "cv" 'php-refactor--convert-local-to-instance-variable
        :n "u"  'php-refactor--optimize-use
        :v "xm" 'php-refactor--extract-method
        :n "rv" 'php-refactor--rename-local-variable

        :prefix "t"
        :n "r" 'phpunit-current-project
        :n "a" 'phpunit-current-class
        :n "s" 'phpunit-current-test))


(@def-package php-extras
  :after php-mode
  :init
  (add-hook 'php-mode-hook 'eldoc-mode)
  :config
  (setq php-extras-eldoc-functions-file (concat doom-cache-dir "/php-extras-eldoc-functions"))

  ;; company will set up itself
  (advice-add 'php-extras-company-setup :override 'ignore)

  ;; Make expensive php-extras generation async
  (unless (file-exists-p (concat php-extras-eldoc-functions-file ".el"))
    (async-start `(lambda ()
                    ,(async-inject-variables "\\`\\(load-path\\|php-extras-eldoc-functions-file\\)$")
                    (require 'php-extras-gen-eldoc)
                    (php-extras-generate-eldoc-1 t))
                 (lambda (_)
                   (load (concat php-extras-eldoc-functions-file ".el"))
                   (message "PHP eldoc updated!")))))


(@def-package php-refactor-mode
  :commands php-refactor-mode
  :init (add-hook 'php-mode-hook 'php-refactor-mode))


(@def-package phpunit
  :commands (phpunit-current-test phpunit-current-class phpunit-current-project))


(@def-package php-boris :commands php-boris)


;;
;; Projects
;;

;; (@def-project laravel "laravel"
;;   :modes (php-mode yaml-mode web-mode nxml-mode js2-mode scss-mode)
;;   :files ("artisan" "server.php"))

;; (defvar php-composer-conf (make-hash-table :test 'equal))
;; (@def-project composer "composer"
;;   :modes (web-mode php-mode)
;;   :files ("composer.json")
;;   :when
;;   (lambda (&rest _)
;;     (let* ((project-path (doom/project-root))
;;            (hash (gethash project-path php-composer-conf))
;;            (package-file (expand-file-name "composer.json" project-path))
;;            deps)
;;       (awhen (and (not hash) (file-exists-p package-file)
;;                   (json-read-file package-file))
;;         (puthash project-path it php-composer-conf)))
;;     t))

