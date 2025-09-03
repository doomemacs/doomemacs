;;; lang/php/config.el -*- lexical-binding: t; -*-

(defvar +php-default-docker-container "php-fpm"
  "The default docker container to run commands in.")

(defvar +php-default-docker-compose "docker-compose.yml"
  "Path to docker-compose file.")

(defvar +php-run-tests-in-docker nil
  "Whether or not to run tests in a docker environment")

;; DEPRECATED: Remove when projectile is replaced with project.el
(after! projectile
  (add-to-list 'projectile-project-root-files "composer.json"))


;;
;;; Packages

(defun +php-common-config (mode)
  (set-docsets! mode "PHP" "PHPUnit" "Laravel" "CakePHP" "CodeIgniter" "Doctrine_ORM")
  (set-repl-handler! mode #'+php/open-repl)
  (set-lookup-handlers! mode :documentation #'php-search-documentation)
  (set-ligatures! mode
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

  (let ((mode-vars-hook (intern (format "%s-local-vars-hook" mode)))
        (mode-map (intern (format "%s-map" mode))))
    (sp-with-modes (ensure-list mode)
      (sp-local-pair "<?"    "?>" :post-handlers '(("| " "SPC" "=") ("||\n[i]" "RET") ("[d2]" "p")))
      (sp-local-pair "<?php" "?>" :post-handlers '(("| " "SPC") ("||\n[i]" "RET"))))

    (when (modulep! +lsp)
      (when (executable-find "php-language-server.php")
        (setq lsp-clients-php-server-command "php-language-server.php"))
      (add-hook mode-vars-hook #'lsp! 'append))

    (map! :localleader
          :map ,mode-map
          :prefix ("t" . "test")
          "r" #'phpunit-current-project
          "a" #'phpunit-current-class
          "s" #'phpunit-current-test)))


(use-package! php-mode
  :defer t
  :config
  (+php-common-config 'php-mode)

  ;; Disable HTML compatibility in php-mode. `web-mode' has superior support for
  ;; php+html. Use the .phtml extension instead.
  (setq php-mode-template-compatibility nil))


(use-package! php-ts-mode
  :when (modulep! +tree-sitter)
  :defer t
  :init
  (set-tree-sitter! '(php-mode php-mode-maybe) 'php-ts-mode
    '((php :url "https://github.com/tree-sitter/tree-sitter-php"
           :rev "v0.23.11"
           :commit "f7cf7348737d8cff1b13407a0bfedce02ee7b046"
           :source-dir "php/src")
      (phpdoc :url "https://github.com/claytonrcarter/tree-sitter-phpdoc"
              :commit "03bb10330704b0b371b044e937d5cc7cd40b4999")
      html css            ; requires :lang (web +tree-sitter)
      javascript jsdoc))  ; requires :lang (javascript +tree-sitter)
  :config
  (+php-common-config 'php-ts-mode))


(use-package! php-refactor-mode
  :hook php-mode
  :hook php-ts-mode
  :config
  (map! :localleader
        :map php-refactor-mode-map
        :prefix "r"
        "cv" #'php-refactor--convert-local-to-instance-variable
        "u"  #'php-refactor--optimize-use
        "xm" #'php-refactor--extract-method
        "rv" #'php-refactor--rename-local-variable))


(use-package! hack-mode
  :when (modulep! +hack)
  :mode "\\.hh\\'")


(use-package! composer
  :defer t
  :init
  (setq composer-directory-to-managed-file (file-name-concat doom-etc-dir "composer/"))
  (defvar +php-common-mode-map (make-sparse-keymap))
  (map! :map +php-common-mode-map
        "c" #'composer
        "i" #'composer-install
        "r" #'composer-require
        "u" #'composer-update
        "d" #'composer-dump-autoload
        "s" #'composer-run-script
        "v" #'composer-run-vendor-bin-command
        "o" #'composer-find-json-file
        "l" #'composer-view-lock-file)
  (map! :after php-mode
        :localleader
        :map php-mode-map
        :desc "composer" "c" +php-common-mode-map)
  (map! :after php-ts-mode
        :localleader
        :map php-ts-mode-map
        :desc "composer" "c" +php-common-mode-map))


;;
;; Projects

(def-project-mode! +php-laravel-mode
  :modes '(php-mode php-ts-mode yaml-mode yaml-ts-mode web-mode nxml-mode js-mode js-ts-mode scss-mode)
  :files (and "artisan" "server.php"))

(def-project-mode! +php-composer-mode
  :modes '(web-mode php-mode php-ts-mode)
  :files ("composer.json"))

(def-project-mode! +phpunit-docker-compose-mode
  :when +php-run-tests-in-docker
  :modes '(php-mode php-ts-mode docker-compose-mode)
  :files (and "phpunit.xml" (or +php-default-docker-compose  "docker-compose.yml"))
  :on-enter
  (setq phpunit-args `("exec" ,+php-default-docker-container "php" "vendor/bin/phpunit")
        phpunit-executable (executable-find "docker-compose"))
  :on-exit
  (setq phpunit-args nil
        phpunit-executable nil))
