;;; lang/elm/config.el -*- lexical-binding: t; -*-

(def-package! elm-mode
  :mode "\\.elm$"
  :config
  (add-hook! 'elm-mode-hook #'(flycheck-mode rainbow-delimiters-mode))
  (set! :company-backend 'elm-mode '(company-elm))
  (setq elm-format-on-save t))


(def-package! flycheck-elm
  :after elm-mode
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-elm-setup))

