;;; lang/haskell/+lsp.el -*- lexical-binding: t; -*-

(use-package! lsp-haskell
  :after lsp-mode
  :preface (add-hook 'haskell-mode-local-vars-hook #'lsp!)
  :config
  ;; Does some strange indentation if it pastes in the snippet
  (setq-hook! 'haskell-mode-hook yas-indent-line 'fixed))
