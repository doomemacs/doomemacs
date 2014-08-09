;; TODO Make more Unity-friendly

(use-package omnisharp :ensure t :defer t
  :config
  (progn
    ;; (setq flycheck-idle-change-delay 2)
    (setq omnisharp-server-executable-path "/Users/hlissner/Omni/OmniSharpServer/OmniSharp/bin/Debug/OmniSharp.exe")))

(use-package shaderlab-mode :defer t
  :mode "\\.shader")

(use-package csharp-mode :ensure t
  :mode "\\.cs\\'"
  :config
  (progn
    (diminish 'abbrev-mode)
    (imap csharp-mode-map (kbd "C-SPC") 'omnisharp-auto-complete))
  :init
  (progn
    (setq csharp-want-imenu nil)
    (add-hook 'csharp-mode-hook (lambda() (omnisharp-mode t) (flycheck-mode t)))))

(provide 'mod-csharp)
