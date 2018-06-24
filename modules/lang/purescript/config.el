;;; lang/purescript/config.el -*- lexical-binding: t; -*-

(after! purescript-mode
  (add-hook! 'purescript-mode-hook
    #'(purescript-indentation-mode
       rainbow-delimiters-mode)))


;; (def-package! flycheck-purescript
;;   :after purescript-mode
;;   :config
;;   (add-hook 'flycheck-mode-hook #'flycheck-purescript-setup))


(def-package! psc-ide
  :hook (purescript-mode . psc-ide-mode))

