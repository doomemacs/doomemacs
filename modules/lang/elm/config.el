;;; lang/elm/config.el -*- lexical-binding: t; -*-

(def-package! elm-mode
  :mode "\\.elm$"
  :config
  (load "elm-mode-autoloads" nil t)
  (add-hook! 'elm-mode-hook #'(flycheck-mode rainbow-delimiters-mode))
  (set! :company-backend 'elm-mode '(company-elm))
  (set! :repl 'elm-mode #'run-elm-interactive)
  (setq elm-format-on-save t))


(def-package! flycheck-elm
  :after (:all flycheck elm-mode)
  :hook (flycheck-mode . flycheck-elm-setup))

