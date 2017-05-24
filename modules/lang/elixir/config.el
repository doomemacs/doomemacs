;;; lang/elixir/config.el

(def-package! elixir-mode
  :mode ("\\.ex$")
  :init
  (add-hook 'elixir-mode-hook #'turn-off-smartparens-mode)
  (add-hook 'elixir-mode-hook #'alchemist)
  :config
  (set! :company-backend 'elixir-mode '(alchemist-company company-yasnippet)))

