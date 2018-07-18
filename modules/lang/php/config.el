;;; lang/php/config.el -*- lexical-binding: t; -*-

(defvar +php--company-backends nil)

;; (def-package! hack-mode
;;   :mode "\\.hh$"
;;   :config
;;   (set-company-backend! 'hack-mode '(company-capf)))


(def-package! php-mode
  :mode "\\.inc\\'"
  :interpreter "php"
  :config
  ;; Disable HTML compatibility in php-mode. `web-mode' has superior support for
  ;; php+html. Use the .phtml
  (setq php-template-compatibility nil)

  (set-repl-handler! 'php-mode #'php-boris)
  (set-lookup-handlers! 'php-mode :documentation #'php-search-documentation)

  ;; ac-php provides custom autocompletion, php-extras provides autocompletion
  ;; for built-in libraries
  (when +php--company-backends
    (set-company-backend! 'php-mode +php--company-backends))

  ;; Use the smallest `sp-max-pair-length' for optimum `smartparens' performance
  (setq-hook! 'php-mode-hook sp-max-pair-length 6)

  (sp-with-modes '(php-mode)
    (sp-local-pair "<?"    "?>" :post-handlers '(("| " "SPC" "=") ("||\n[i]" "RET") ("[d2]" "p")))
    (sp-local-pair "<?php" "?>" :post-handlers '(("| " "SPC") ("||\n[i]" "RET"))))

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


(def-package! php-refactor-mode
  :hook php-mode)


(def-package! php-extras
  :after php-mode
  :init
  ;; company will set up itself
  (advice-add #'php-extras-company-setup :override #'ignore)
  (add-to-list '+php--company-backends 'php-extras-company nil #'eq)
  :config
  (setq php-extras-eldoc-functions-file
        (concat doom-etc-dir "php-extras-eldoc-functions"))

  ;; Make expensive php-extras generation async
  (unless (file-exists-p (concat php-extras-eldoc-functions-file ".el"))
    (message "Generating PHP eldoc files...")
    (require 'async)
    (async-start (lambda ()
                   ,(async-inject-variables "\\`\\(load-path\\|php-extras-eldoc-functions-file\\)$")
                   (require 'php-extras-gen-eldoc)
                   (php-extras-generate-eldoc-1 t))
                 (lambda (_)
                   (load (concat php-extras-eldoc-functions-file ".el"))
                   (message "PHP eldoc updated!")))))


(def-package! company-php
  :when (featurep! :completion company)
  :commands (ac-php-remake-tags ac-php-remake-tags-all)
  :hook (php-mode . ac-php-core-eldoc-setup)
  :init
  (add-to-list '+php--company-backends 'company-ac-php-backend nil #'eq)
  :config (setq ac-php-tags-path (concat doom-cache-dir "ac-php/")))


;;
;; Projects
;;

(def-project-mode! +php-laravel-mode
  :modes (php-mode yaml-mode web-mode nxml-mode js2-mode scss-mode)
  :files (and "artisan" "server.php"))

(def-project-mode! +php-composer-mode
  :modes (web-mode php-mode)
  :files ("composer.json"))

