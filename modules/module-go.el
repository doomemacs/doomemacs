;;; module-go.el

(use-package go-mode
  :mode "\\.go$"
  :interpreter "go"
  :init
  (def-builder! go-mode "go build")
  (def-company-backend! go-mode (go yasnippet))
  (def-repl! go-mode gorepl-run)
  (add-hook! go-mode '(emr-initialize flycheck-mode go-eldoc-setup))

  :config
  (map! :map go-mode-map
        :n "gd" 'godef-jump
        :n "gD" 'godef-describe
        (:localleader
          :n "p" 'helm-go-package
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
        '((go-remove-unused-imports "Remove unushed imports" nil)
          (gofmt                    "Format code" nil))))

(use-package go-eldoc :after go-mode)

(use-package company-go :after go-mode)

(use-package gorepl-mode :commands (gorepl-run gorepl-run-load-current-file))

(use-package helm-go-package :commands helm-go-package)

(provide 'module-go)
;;; module-go.el ends here
