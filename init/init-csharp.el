(use-package omnisharp :defer t
  :config
  (progn
    (setq omnisharp-server-executable-path "~/Dropbox/projects/lib/Omnisharp/server/OmniSharp/bin/Debug/OmniSharp.exe")

    (bind 'normal omnisharp-mdoe-map
          "gd" 'omnisharp-go-to-definition
          "")

    (after "company"
      (company--backend-on 'csharpmode 'company-omnisharp)
      (add-hook 'csharp-mode-hook 'eldoc-mode))
    ))

(use-package csharp-mode :mode "\\.cs$")
;; (use-package csharp-mode
;;   :mode "\\.cs$"
;;   :config
;;   (bind 'insert csharp-mode-map (kbd "C-SPC") 'omnisharp-auto-complete))
;;   :init
;;   (add-hook! 'csharp-mode-hook (omnisharp-mode t) (flycheck-mode t)))

;; unity shaders
(use-package shaderlab-mode :mode "\\.shader$")

;; TODO Make more Unity-friendly

(provide 'init-csharp)
;;; init-csharp.el ends here
