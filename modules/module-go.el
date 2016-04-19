;;; module-go.el

(use-package gorepl-mode :commands (gorepl-run gorepl-run-load-current-file))

(use-package go-mode
  :mode "\\.go$"
  :interpreter "go"
  :init
  (add-hook! go-mode '(emr-initialize flycheck-mode))
  (define-builder! go-mode "go build")
  (define-repl! go-mode gorepl-run)

  :config
  (after! emr
    (mapc (lambda (x)
            (let ((command-name (car x))
                  (title (cadr x))
                  (region-p (caddr x))
                  predicate)
              (setq predicate (cond ((eq region-p 'both) nil)
                                    (t (if region-p
                                           (lambda () (use-region-p))
                                         (lambda () (not (use-region-p)))))))
              (emr-declare-command (intern (symbol-name command-name))
                :title title :modes 'go-mode :predicate predicate)))
          '((go-remove-unused-imports "Remove unushed imports" nil)
            (gofmt                    "Format code" nil))))

  (after! helm
    (use-package helm-go-package :defer t))

  (require 'go-eldoc)
  (add-hook 'go-mode-hook 'go-eldoc-setup)

  (require 'company-go)
  (define-company-backend! go-mode (go yasnippet))

  (map! :map go-mode-map
        :n "gd" 'godef-jump
        :n "gD" 'godef-describe
        (:localleader
         :n "p" 'helm-go-package
         :n "tr" 'narf:go-test-run-all
         :n "ta" 'narf:go-test-run-all
         :n "ts" 'narf:go-test-run-package)))

(provide 'module-go)
;;; module-go.el ends here
