(use-package go-mode
  :mode "\\.go$"
  :interpreter "go"
  :config
  (progn
    (bind 'normal go-mode-map "gd" 'godef-jump)

    (use-package company-go
      :config
      (company--backend-on 'go-mode-hook 'company-go 'company-yasnippet))))


(provide 'init-go)
;;; init-go.el ends here
