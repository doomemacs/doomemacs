;;; +lsp.el --- description -*- lexical-binding: t; -*-
;;;###if (featurep! +lsp)

(def-package! lsp-haskell
  :after haskell-mode
  :init
  (add-hook 'haskell-mode-hook #'lsp-haskell-enable)
  (add-hook 'haskell-mode-hook 'flycheck-mode))
