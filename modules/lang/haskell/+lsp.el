;;; lang/haskell/+lsp.el -*- lexical-binding: t; -*-

(def-package! lsp-haskell
  :after haskell-mode
  :init (add-hook 'haskell-mode-hook #'lsp!)
  :config
  ;; Does some strange indentation if it pastes in the snippet
  (setq-hook! 'haskell-mode-hook yas-indent-line 'fixed))
