;;; lang/elixir/config.el

(def-package! elixir-mode
  :mode ("\\.ex$")
  :init
  (add-hook 'elixir-mode-hook #'turn-off-smartparens-mode)
  :config
  (set! :company-backend 'elixir-mode '(company-yasnippet))

)


(def-package! company-elixir
  :when (featurep! :completion company)
  :after elixir-mode)
