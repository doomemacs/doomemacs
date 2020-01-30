;;; lang/purescript/config.el -*- lexical-binding: t; -*-

(after! purescript-mode
  (add-hook! 'purescript-mode-hook
             #'purescript-indentation-mode
             #'rainbow-delimiters-mode)
  (set-lookup-handlers! 'purescript-mode
    :definition #'psc-ide-goto-definition
    :documentation #'purescript-pursuit))


;; (use-package! flycheck-purescript
;;   :after purescript-mode
;;   :config
;;   (add-hook 'flycheck-mode-hook #'flycheck-purescript-setup))

(cond ((featurep! +psc-ide) (load! "+psc-ide"))
      ((featurep! +lsp) (load! "+lsp")))
