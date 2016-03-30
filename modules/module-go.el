;;; module-go.el

(use-package go-mode
  :mode "\\.go$"
  :interpreter "go"
  :init
  (add-hook! go-mode '(emr-initialize narf|flycheck-enable-maybe))
  :config
  (define-builder! go-mode "go build")

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

  (use-package go-eldoc
    :config (add-hook 'go-mode-hook 'go-eldoc-setup))

  (use-package company-go
    :config
    (define-company-backend! go-mode (go yasnippet)))

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
