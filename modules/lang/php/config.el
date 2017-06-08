;;; lang/php/config.el -*- lexical-binding: t; -*-

;; (def-package! hack-mode
;;   :mode "\\.hh$"
;;   :config
;;   (set! :company-backend 'hack-mode '(company-capf)))


(def-package! php-mode
  :mode ("\\.php[s345]?$" "\\.inc$")
  :interpreter "php"
  :config
  (add-hook! 'php-mode-hook
    #'(ac-php-core-eldoc-setup flycheck-mode))

  (setq php-template-compatibility nil)

  (set! :repl 'php-mode #'php-boris)

  ;; ac-php provides custom autocompletion, php-extras provides autocompletion
  ;; for built-in libraries
  (set! :company-backend 'php-mode '(company-ac-php-backend php-extras-company))

  ;; default is 10; this optimizes `smartparens' performance, but limits sp
  ;; pairs to 6 characters.
  (add-hook! php-mode (setq-local sp-max-pair-length 6))

  (sp-with-modes '(php-mode)
    (sp-local-pair "/* "    "*/" :post-handlers '(("||\n[i] " "RET") ("| " "SPC")))
    (sp-local-pair "<? "    " ?>")
    (sp-local-pair "<?php " " ?>")
    (sp-local-pair "<?="    " ?>")
    (sp-local-pair "<?"    "?>"   :when '(("RET")) :post-handlers '("||\n[i]"))
    (sp-local-pair "<?php" "?>"   :when '(("RET")) :post-handlers '("||\n[i]")))

  (map! :map php-mode-map
        :localleader
        (:prefix "r"
          :n "cv" #'php-refactor--convert-local-to-instance-variable
          :n "u"  #'php-refactor--optimize-use
          :v "xm" #'php-refactor--extract-method
          :n "rv" #'php-refactor--rename-local-variable)
        (:prefix "t"
          :n "r"  #'phpunit-current-project
          :n "a"  #'phpunit-current-class
          :n "s"  #'phpunit-current-test)))


(def-package! php-extras
  :after php-mode
  :init
  ;; company will set up itself
  (advice-add #'php-extras-company-setup :override #'ignore)
  :config
  (setq php-extras-eldoc-functions-file
        (concat doom-etc-dir "php-extras-eldoc-functions"))

  ;; Make expensive php-extras generation async
  (unless (file-exists-p (concat php-extras-eldoc-functions-file ".el"))
    (message "Generating PHP eldoc files...")
    (async-start `(lambda ()
                    ,(async-inject-variables "\\`\\(load-path\\|php-extras-eldoc-functions-file\\)$")
                    (require 'php-extras-gen-eldoc)
                    (php-extras-generate-eldoc-1 t))
                 (lambda (_)
                   (load (concat php-extras-eldoc-functions-file ".el"))
                   (message "PHP eldoc updated!")))))


(def-package! php-refactor-mode
  :commands php-refactor-mode
  :init (add-hook 'php-mode-hook #'php-refactor-mode))


(def-package! phpunit
  :commands (phpunit-current-test phpunit-current-class phpunit-current-project))


(def-package! php-boris :commands php-boris)


(def-package! company-php
  :when (featurep! :completion company)
  :commands (company-ac-php-backend ac-php-remake-tags ac-php-remake-tags-all ac-php-core-eldoc-setup)
  :config
  (unless (executable-find "phpctags")
    (warn "php-mode: phpctags isn't installed, auto-completion will be gimped"))

  (setq ac-php-tags-path (concat doom-cache-dir "ac-php/")))


;;
;; Projects
;;

(def-project-mode! +php-laravel-mode
  :modes (php-mode yaml-mode web-mode nxml-mode js2-mode scss-mode)
  :files (and "artisan" "server.php"))

(def-project-mode! +php-composer-mode
  :modes (web-mode php-mode)
  :files "composer.json")

