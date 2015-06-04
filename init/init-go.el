(use-package go-mode
  :mode "\\.go$"
  :interpreter "go"
  :config
  (progn
    (bind normal :map go-mode-map "gd" 'godef-jump)

    (use-package company-go
      :config
      (narf/add-company-backend go-mode (company-go company-yasnippet)))))


(provide 'init-go)
;;; init-go.el ends here
