;;; lang/elm/config.el -*- lexical-binding: t; -*-

;; `elm-mode'
(setq elm-format-on-save t)

(add-hook! 'elm-mode-hook #'(flycheck-mode rainbow-delimiters-mode))

(set! :company-backend 'elm-mode 'company-elm)
(set! :repl 'elm-mode #'run-elm-interactive)


(def-package! flycheck-elm
  :when (featurep! :feature syntax-checker)
  :after elm-mode
  :hook (flycheck-mode . flycheck-elm-setup))

