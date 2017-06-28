;;; lang/elixir/config.el -*- lexical-binding: t; -*-

(def-package! elixir-mode
  :mode "\\.exs?$"
  :init
  (add-hook! 'elixir-mode-hook #'(turn-off-smartparens-mode alchemist))
  :config
  (set! :company-backend 'elixir-mode '(alchemist-company company-yasnippet)))

