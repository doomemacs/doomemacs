;;; +lsp.el --- description -*- lexical-binding: t; -*-
;;;###if (featurep! +lsp)

(def-package! lsp-haskell
  :hook (haskell-mode . lsp-haskell-enable))
