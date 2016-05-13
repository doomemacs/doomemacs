;;; module-go.el

(use-package go-mode
  :mode "\\.go$"
  :interpreter "go"
  :init
  (add-hook 'go-mode-hook 'flycheck-mode)
  (add-hook! go-mode (add-hook 'before-save-hook 'gofmt-before-save))
  :config
  (def-builder! go-mode "go build")
  (def-company-backend! go-mode (go yasnippet))
  (def-repl! go-mode gorepl-run)
  (map! :map go-mode-map
        :n "gd" 'godef-jump
        (:leader :n "h" 'godef-describe)
        (:localleader
          :n "p"  'helm-go-package
          :n "tr" 'narf:go-test-run-all
          :n "ta" 'narf:go-test-run-all
          :n "ts" 'narf:go-test-run-package))

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
        '((go-remove-unused-imports "Remove unused imports" nil)
          (gofmt                    "Format code" nil))))

(use-package go-eldoc :after go-mode
  :init (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package company-go :after go-mode)

(use-package gorepl-mode :commands (gorepl-run gorepl-run-load-current-file))

(use-package helm-go-package :commands helm-go-package)

(provide 'module-go)
;;; module-go.el ends here
