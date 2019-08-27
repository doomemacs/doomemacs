;;; lang/haskell/+lsp.el -*- lexical-binding: t; -*-

(use-package! lsp-haskell
  :after haskell-mode
  :init (add-hook 'haskell-mode-hook #'lsp!)
  :config
  (when IS-MAC
    (setq lsp-haskell-process-path-hie "hie-wrapper"))
  ;; Does some strange indentation if it pastes in the snippet
  (setq-hook! 'haskell-mode-hook yas-indent-line 'fixed))
