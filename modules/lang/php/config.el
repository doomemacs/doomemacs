;;; lang/php/config.el -*- lexical-binding: t; -*-

(def-package! php-mode
  :mode "\\.inc\\'"
  :config
  ;; Disable HTML compatibility in php-mode. `web-mode' has superior support for
  ;; php+html. Use the .phtml
  (setq php-template-compatibility nil)

  (set-repl-handler! 'php-mode #'php-boris)
  (set-lookup-handlers! 'php-mode :documentation #'php-search-documentation)

  ;; `+php-company-backend' uses `company-phpactor', `php-extras-company' or
  ;; `company-dabbrev-code', in that order.
  (set-company-backend! 'php-mode '+php-company-backend 'company-dabbrev-code)

  ;; Use the smallest `sp-max-pair-length' for optimum `smartparens' performance
  (setq-hook! 'php-mode-hook sp-max-pair-length 5)

  (sp-with-modes '(php-mode)
    (sp-local-pair "<?"    "?>" :post-handlers '(("| " "SPC" "=") ("||\n[i]" "RET") ("[d2]" "p")))
    (sp-local-pair "<?php" "?>" :post-handlers '(("| " "SPC") ("||\n[i]" "RET"))))

  (map! :map php-mode-map
        :localleader
        :prefix "t"
        :n "r"  #'phpunit-current-project
        :n "a"  #'phpunit-current-class
        :n "s"  #'phpunit-current-test))


(def-package! phpactor
  :after php-mode
  :config
  (set-lookup-handlers! 'php-mode
    :definition #'phpactor-goto-definition)

  ;; TODO PR these for phpactor.el?
  ;; company-phpactor breaks company if executable doesn't exist
  (defun +php*company-phpactor-fail-silently (orig-fn &rest args)
    (when (phpactor-find-executable)
      (apply orig-fn args)))
  (advice-add #'company-phpactor :around #'+php*company-phpactor-fail-silently)

  ;; `phpactor-get-working-dir' throws stringp errors if not in a project.
  (defun +php*project-root (&rest _)
    (setq phpactor-working-dir
          (or phpactor-working-dir
              (php-project-get-root-dir)
              (doom-project-root))))
  (advice-add #'phpactor-get-working-dir :before #'+php*project-root)

  (map! :map php-mode-map
        :localleader
        :prefix "r"
        :n "cc" #'phpactor-copy-class
        :n "mc" #'phpactor-move-class
        :v "oi" #'phpactor-offset-info
        :n "t"  #'phpactor-transform
        :n "ic" #'phpactor-import-class))


(def-package! php-refactor-mode
  :hook php-mode
  :config
  (map! :map php-refactor-mode-map
        :localleader
        :prefix "r"
        :n "cv" #'php-refactor--convert-local-to-instance-variable
        :n "u"  #'php-refactor--optimize-use
        :v "xm" #'php-refactor--extract-method
        :n "rv" #'php-refactor--rename-local-variable))


(def-package! php-extras
  :after php-mode
  :preface
  ;; We'll set up company support ourselves
  (advice-add #'php-extras-company-setup :override #'ignore)
  :config
  (setq php-extras-eldoc-functions-file
        (concat doom-etc-dir "php-extras-eldoc-functions"))
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


(def-package! hack-mode
  :when (featurep! +hack)
  :mode "\\.hh$")


;;
;; Projects
;;

(def-project-mode! +php-laravel-mode
  :modes (php-mode yaml-mode web-mode nxml-mode js2-mode scss-mode)
  :files (and "artisan" "server.php"))

(def-project-mode! +php-composer-mode
  :modes (web-mode php-mode)
  :files ("composer.json"))

