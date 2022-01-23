;;; lang/php/config.el -*- lexical-binding: t; -*-

(defvar +php--company-backends nil
  "List of company backends to use in `php-mode'.")

(defvar +php-default-docker-container "php-fpm"
  "The default docker container to run commands in.")

(defvar +php-default-docker-compose "docker-compose.yml"
  "Path to docker-compose file.")

(defvar +php-run-tests-in-docker nil
  "Whether or not to run tests in a docker environment")

(after! projectile
  (add-to-list 'projectile-project-root-files "composer.json"))


;;
;;; Packages

(use-package! php-mode
  :mode "\\.inc\\'"
  :hook (php-mode . rainbow-delimiters-mode)
  :config
  ;; Disable HTML compatibility in php-mode. `web-mode' has superior support for
  ;; php+html. Use the .phtml extension instead.
  (setq php-mode-template-compatibility nil)

  (set-docsets! 'php-mode "PHP" "PHPUnit" "Laravel" "CakePHP" "CodeIgniter" "Doctrine_ORM")
  (set-repl-handler! 'php-mode #'+php/open-repl)
  (set-lookup-handlers! 'php-mode :documentation #'php-search-documentation)
  (set-formatter! 'php-mode #'php-cs-fixer-fix)
  (set-ligatures! 'php-mode
    ;; Functional
    :lambda "function()" :lambda "fn"
    :def "function"
    ;; Types
    :null "null"
    :true "true" :false "false"
    :int "int" :float "float"
    :str "string"
    :bool "list"
    ;; Flow
    :not "!"
    :and "&&" :and "and"
    :or "||" :or "or"
    :for "for"
    :return "return"
    :yield "use")

  (if (not (featurep! +lsp))
      ;; `+php-company-backend' uses `company-phpactor', `php-extras-company' or
      ;; `company-dabbrev-code', in that order.
      (when +php--company-backends
        (set-company-backend! 'php-mode
          (cons :separate +php--company-backends)
          'company-dabbrev-code))
    (when (executable-find "php-language-server.php")
      (setq lsp-clients-php-server-command "php-language-server.php"))
    (add-hook 'php-mode-local-vars-hook #'lsp!))

  ;; Use the smallest `sp-max-pair-length' for optimum `smartparens' performance
  (setq-hook! 'php-mode-hook sp-max-pair-length 5)

  (sp-with-modes '(php-mode)
    (sp-local-pair "<?"    "?>" :post-handlers '(("| " "SPC" "=") ("||\n[i]" "RET") ("[d2]" "p")))
    (sp-local-pair "<?php" "?>" :post-handlers '(("| " "SPC") ("||\n[i]" "RET"))))

  (map! :localleader
        :map php-mode-map
        :prefix ("t" . "test")
        "r" #'phpunit-current-project
        "a" #'phpunit-current-class
        "s" #'phpunit-current-test))


(use-package! phpactor
  :unless (featurep! +lsp)
  :after php-mode
  :init
  (add-to-list '+php--company-backends #'company-phpactor nil 'eq)
  :config
  (set-lookup-handlers! 'php-mode
    :definition #'phpactor-goto-definition)
  (map! :localleader
        :map php-mode-map
        :prefix ("r" . "refactor")
        "cc" #'phpactor-copy-class
        "mc" #'phpactor-move-class
        "oi" #'phpactor-offset-info
        "t"  #'phpactor-transform
        "ic" #'phpactor-import-class))


(use-package! php-refactor-mode
  :hook php-mode
  :config
  (map! :localleader
        :map php-refactor-mode-map
        :prefix "r"
        "cv" #'php-refactor--convert-local-to-instance-variable
        "u"  #'php-refactor--optimize-use
        "xm" #'php-refactor--extract-method
        "rv" #'php-refactor--rename-local-variable))


(use-package! php-extras
  :after php-mode
  :preface
  ;; We'll set up company support ourselves
  (advice-add #'php-extras-company-setup :override #'ignore)
  :init
  (add-to-list '+php--company-backends #'php-extras-company)
  :config
  (setq php-extras-eldoc-functions-file
        (concat doom-etc-dir "php-extras-eldoc-functions"))
  ;; Silence warning if `php-extras-eldoc-functions-file' hasn't finished
  ;; generating yet.
  (defun php-extras-load-eldoc ()
    (require 'php-extras-eldoc-functions php-extras-eldoc-functions-file t))
  ;; Make expensive php-extras generation async
  (unless (file-exists-p (concat php-extras-eldoc-functions-file ".el"))
    (message "Generating PHP eldoc files...")
    (require 'async)
    (async-start `(lambda ()
                    ,(async-inject-variables "\\`\\(load-path\\|php-extras-eldoc-functions-file\\)$")
                    (require 'php-extras-gen-eldoc)
                    (php-extras-generate-eldoc-1 t))
                 (lambda (_)
                   (load (concat php-extras-eldoc-functions-file ".el"))
                   (message "PHP eldoc updated!")))))


(use-package! hack-mode
  :when (featurep! +hack)
  :mode "\\.hh$")


(use-package! composer
  :defer t
  :init
  (map! :after php-mode
        :localleader
        :map php-mode-map
        :prefix ("c" . "composer")
        "c" #'composer
        "i" #'composer-install
        "r" #'composer-require
        "u" #'composer-update
        "d" #'composer-dump-autoload
        "s" #'composer-run-script
        "v" #'composer-run-vendor-bin-command
        "o" #'composer-find-json-file
        "l" #'composer-view-lock-file))


;;
;; Projects

(def-project-mode! +php-laravel-mode
  :modes '(php-mode yaml-mode web-mode nxml-mode js2-mode scss-mode)
  :files (and "artisan" "server.php"))

(def-project-mode! +php-composer-mode
  :modes '(web-mode php-mode)
  :files ("composer.json"))

(def-project-mode! +phpunit-docker-compose-mode
  :when +php-run-tests-in-docker
  :modes '(php-mode docker-compose-mode)
  :files (and "phpunit.xml" (or +php-default-docker-compose  "docker-compose.yml"))
  :on-enter
  (setq phpunit-args `("exec" ,+php-default-docker-container "php" "vendor/bin/phpunit")
        phpunit-executable (executable-find "docker-compose"))
  :on-exit
  (setq phpunit-args nil
        phpunit-executable nil))
