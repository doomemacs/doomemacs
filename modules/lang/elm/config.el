;;; lang/elm/config.el

(def-package! elm-mode
  :mode "\\.elm$"
  :init
  (add-hook 'elm-mode-hook #'flycheck-mode)
  (add-hook 'elm-mode-hook #'rainbow-delimiters-mode)
  :config
  (set! :company-backend 'elm-mode '(company-elm))
  (setq elm-format-on-save t)
)

(def-package! flycheck-elm
  :after elm-mode
  :config
  (add-hook! 'flycheck-mode-hook #'flycheck-elm-setup)
)

