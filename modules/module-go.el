;;; module-go.el

(use-package go-mode
  :mode "\\.go$"
  :interpreter "go"
  :init
  (add-hook! go-mode '(emr-initialize flycheck-mode))
  (build-for! go-mode "go build")
  :config
  (bind! :map go-mode-map
         :n "gd" 'godef-jump
         :n "gD" 'godef-describe
         :n "tr" 'narf:go-test-run-all
         :n "ta" 'narf:go-test-run-all
         :n "ts" 'narf:go-test-run-package)

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
          (gofmt                    "Format code" nil)))

  (use-package go-eldoc
    :config (add-hook! go-mode 'go-eldoc-setup))

  (use-package company-go
    :config
    (add-company-backend! go-mode (go yasnippet))))

(provide 'module-go)
;;; module-go.el ends here
