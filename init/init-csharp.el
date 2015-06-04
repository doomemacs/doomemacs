(use-package csharp-mode :mode "\\.cs$"
  :init
  (add-hook 'csharp-mode-hook 'flycheck-mode)
  :config
  (progn
    (use-package omnisharp
      :defer t
      :config
      (progn
        (setq omnisharp-server-executable-path
              "~/Dropbox/projects/lib/Omnisharp/server/OmniSharp/bin/Debug/OmniSharp.exe")

        (bind :normal :map omnisharp-mode-map
              "gd" 'omnisharp-go-to-definition)

        (after "company"
          (narf/add-company-backend csharp-mode (company-omnisharp))
          (add-hook 'csharp-mode-hook 'turn-on-eldoc-mode))))))

;; unity shaders
(use-package shaderlab-mode :mode "\\.shader$")


(provide 'init-csharp)
;;; init-csharp.el ends here
