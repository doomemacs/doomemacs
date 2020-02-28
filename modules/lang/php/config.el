;;; lang/php/config.el -*- lexical-binding: t; -*-

(defvar +php--company-backends nil
  "List of company backends to use in `php-mode'.")

(after! projectile
  (add-to-list 'projectile-project-root-files "composer.json"))


;;
;;; Packages

(use-package! php-mode
  :mode "\\.inc\\'"
  :config
  ;; Disable HTML compatibility in php-mode. `web-mode' has superior support for
  ;; php+html. Use the .phtml
  (setq php-template-compatibility nil)

  (set-docsets! 'php-mode "PHP" "PHPUnit" "Laravel" "CakePHP" "CodeIgniter" "Doctrine_ORM")
  (set-repl-handler! 'php-mode #'php-boris)
  (set-lookup-handlers! 'php-mode :documentation #'php-search-documentation)
  (set-formatter! 'php-mode #'php-cs-fixer-fix)

  (if (featurep! +lsp)
      (add-hook 'php-mode-local-vars-hook #'lsp!)
    ;; `+php-company-backend' uses `company-phpactor', `php-extras-company' or
    ;; `company-dabbrev-code', in that order.
    (when +php--company-backends
      (set-company-backend! 'php-mode
        (cons :separate +php--company-backends)
        'company-dabbrev-code)))

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


;;
;; Projects

(def-project-mode! +php-laravel-mode
  :modes '(php-mode yaml-mode web-mode nxml-mode js2-mode scss-mode)
  :files (and "artisan" "server.php"))

(def-project-mode! +php-composer-mode
  :modes '(web-mode php-mode)
  :files ("composer.json"))
