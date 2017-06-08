;;; lang/purescript/config.el -*- lexical-binding: t; -*-

(def-package! purescript-mode
  :mode "\\.purs$"
  :config
  (add-hook! 'purescript-mode-hook
    #'(flycheck-mode purescript-indentation-mode rainbow-delimiters-mode))

  (load "purescript-mode-autoloads" nil t))

;; (def-package! flycheck-purescript
;;   :after purescript-mode
;;   :config
;;   (add-hook 'flycheck-mode-hook #'flycheck-purescript-setup))

(def-package! psc-ide
  :after purescript-mode
  :config
  (add-hook 'purescript-mode-hook #'psc-ide-mode))

