;;; lang/haskell/+lsp.el -*- lexical-binding: t; -*-

(use-package! lsp-haskell
  :after lsp-clients
  :preface (add-hook 'haskell-mode-local-vars-hook #'lsp!)
  :config
  (when IS-MAC
    (setq lsp-haskell-process-path-hie "hie-wrapper"))
  (when (featurep! +ghcide)
    (setq lsp-haskell-process-path-hie "ghcide"
          lsp-haskell-process-args-hie nil))
  ;; Does some strange indentation if it pastes in the snippet
  (setq-hook! 'haskell-mode-hook yas-indent-line 'fixed))
