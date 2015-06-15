;;; module-go.el

(use-package go-mode
  :mode "\\.go$"
  :interpreter "go"
  :config
  (bind! :map go-mode-map :n "gd" 'godef-jump)

  (use-package company-go
    :config
    (add-company-backend! go-mode (go yasnippet))))

(provide 'module-go)
;;; module-go.el ends here
