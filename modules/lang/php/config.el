;;; lang/php/config.el -*- lexical-binding: t; -*-

(defvar +php--company-backends nil
  "List of company backends to use in `php-mode' and `php-ts-mode'.")

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

  (let ((mode-hook (intern (format "%s-hook" mode)))
        (mode-vars-hook (intern (format "%s-local-vars-hook" mode)))
        (mode-map (symbol-value (intern (format "%s-map" mode)))))
    (sp-with-modes (ensure-list mode)
      (sp-local-pair "<?"    "?>" :post-handlers '(("| " "SPC" "=") ("||\n[i]" "RET") ("[d2]" "p")))
      (sp-local-pair "<?php" "?>" :post-handlers '(("| " "SPC") ("||\n[i]" "RET"))))

    (if (modulep! -lsp)
        ;; `+php-company-backend' uses `php-extras-company' or
        ;; `company-dabbrev-code', in that order.
        (when +php--company-backends
          (set-company-backend! mode
            (cons :separate +php--company-backends)
            'company-dabbrev-code))
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
  :mode "\\.inc\\'"
  :config
  (+php-common-config 'php-mode)

  ;; Disable HTML compatibility in php-mode. `web-mode' has superior support for
  ;; php+html. Use the .phtml extension instead.
  (setq php-mode-template-compatibility nil))


(use-package! php-ts-mode
  :when (modulep! +tree-sitter)
  :when (fboundp 'php-ts-mode) ; 30.1+ only
  :defer t
  :init
  (set-tree-sitter! 'php-mode 'php-ts-mode
    '((php :url "https://github.com/tree-sitter/tree-sitter-php"
       :rev "v0.23.11"
       :source-dir "php/src")
      (phpdoc :url "https://github.com/claytonrcarter/tree-sitter-phpdoc")))
  :config
  ;; HACK: Rely on `major-mode-remap-defaults'.
  (cl-callf2 rassq-delete-all 'php-ts-mode auto-mode-alist)
  (cl-callf2 rassq-delete-all 'php-ts-mode interpreter-mode-alist)

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


(use-package! php-extras
  :after (:or php-mode php-ts-mode)
  :preface
  (setq php-extras-eldoc-functions-file
        (concat doom-profile-cache-dir "php-extras-eldoc-functions"))
  ;; We'll set up company support ourselves
  (advice-add #'php-extras-company-setup :override #'ignore)
  (add-to-list '+php--company-backends #'php-extras-company)
  :config
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
        :map php-mode-map
        :desc "composer" "c" +php-common-mode-map)
  (map! :after php-ts-mode
        :map php-ts-mode-map
        :desc "composer" "c" +php-common-mode-map))


;;
;; Projects

(def-project-mode! +php-laravel-mode
  :modes '(php-mode php-ts-mode yaml-mode web-mode nxml-mode js2-mode scss-mode)
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
