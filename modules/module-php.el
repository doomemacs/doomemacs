;;; module-php.el

(use-package hack-mode
  :mode "\\.hh$"
  :config (def-company-backend! hack-mode (capf)))

(use-package php-mode
  :mode ("\\.php[s345]?$" "\\.inc$" )
  :interpreter "php"
  :init
  (add-hook 'php-mode-hook 'flycheck-mode)
  (setq php-template-compatibility nil
        php-extras-eldoc-functions-file (concat doom-temp-dir "/php-extras-eldoc-functions"))

  :config
  (def-repl! php-mode php-boris)
  (def-company-backend! php-mode '(php-extras-company company-yasnippet))

  (map! :map php-mode-map (:localleader :nv ";" 'doom/append-semicolon))

  (sp-with-modes '(php-mode)
    (sp-local-pair "/*"    "*/" :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
    (sp-local-pair "/**"   "*/" :post-handlers '(("||\n[i]" "RET") ("||\n[i]" "SPC")))
    (sp-local-pair "<? "    " ?>")
    (sp-local-pair "<?php " " ?>")
    (sp-local-pair "<?="    " ?>")
    (sp-local-pair "<?"    "?>"   :when '(("RET")) :post-handlers '("||\n[i]"))
    (sp-local-pair "<?php" "?>"   :when '(("RET")) :post-handlers '("||\n[i]"))))

(use-package php-extras
  :after php-mode
  :init (add-hook 'php-mode-hook 'turn-on-eldoc-mode)
  :config
  (defun php-extras-company-setup ()) ;; company will set up itself
  ;; Generate php-extras documentation and completion asynchronously
  (unless (file-exists-p (concat php-extras-eldoc-functions-file ".el"))
    (async-start `(lambda ()
                    ,(async-inject-variables "\\`\\(load-path\\|php-extras-eldoc-functions-file\\)$")
                    (require 'php-extras-gen-eldoc)
                    (php-extras-generate-eldoc-1 t))
                 (lambda (_)
                   (load (concat php-extras-eldoc-functions-file ".el"))
                   (message "PHP eldoc updated!")))))

(use-package php-refactor-mode
  :after php-mode
  :init (add-hook 'php-mode-hook 'php-refactor-mode)
  :config
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
          (rename-local-variable              "rename local variable"   nil))))

(use-package phpunit
  :commands (phpunit-current-test phpunit-current-class phpunit-current-project)
  :config
  (map! :map php-mode-map
        (:localleader
          :n "tr" 'phpunit-current-project
          :n "ta" 'phpunit-current-class
          :n "ts" 'phpunit-current-test)))

(use-package php-boris :commands php-boris) ; PHP REPL

;;
(def-project-type! laravel "laravel"
  :modes (php-mode yaml-mode web-mode nxml-mode js2-mode scss-mode)
  :files ("artisan" "server.php"))

(defvar php-composer-conf (make-hash-table :test 'equal))
(def-project-type! composer "composer"
  :modes (web-mode php-mode)
  :files ("composer.json")
  :when
  (lambda (&rest _)
    (let* ((project-path (doom/project-root))
           (hash (gethash project-path php-composer-conf))
           (package-file (f-expand "composer.json" project-path))
           deps)
      (awhen (and (not hash) (f-exists? package-file)
                  (json-read-file package-file))
        (puthash project-path it php-composer-conf)))
    t))

(provide 'module-php)
;;; module-php.el ends here
