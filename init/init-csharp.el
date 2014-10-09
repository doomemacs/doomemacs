(provide 'init-csharp)

;; (use-package omnisharp :defer t
;;   :config
;;   (progn
;;     (setq omnisharp-server-executable-path "~/Dropbox/projects/lib/Omnisharp/server/OmniSharp/bin/Debug/OmniSharp.exe")
;;     (use-package company
;;       :config
;;       (add-to-list 'company-backends 'company-omnisharp))))

(use-package csharp-mode :mode "\\.cs$")
;; (use-package csharp-mode
;;   :mode "\\.cs$"
;;   :config
;;   (imap csharp-mode-map (kbd "C-SPC") 'omnisharp-auto-complete))
;;   :init
;;   (add-hook! 'csharp-mode-hook (omnisharp-mode t) (flycheck-mode t)))

;; unity shaders
(use-package shaderlab-mode :mode "\\.shader$")

;; TODO Make more Unity-friendly
