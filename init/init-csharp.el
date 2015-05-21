(use-package csharp-mode :mode "\\.cs$"
  :config
  (progn
    (after "flycheck" (add-hook 'csharp-mode-hook 'flycheck-mode))
    (use-package omnisharp
      :defer t
      :config
      (progn
        (setq omnisharp-server-executable-path
              "~/Dropbox/projects/lib/Omnisharp/server/OmniSharp/bin/Debug/OmniSharp.exe")

        (bind 'normal omnisharp-mode-map
              "gd" 'omnisharp-go-to-definition)

        (after "company"
               (company--backend-on 'csharp-mode-hook 'company-omnisharp)
               (add-hook 'csharp-mode-hook 'turn-on-eldoc-mode))))))

;; unity shaders
(use-package shaderlab-mode :mode "\\.shader$")

(provide 'init-csharp)
;;; init-csharp.el ends here
