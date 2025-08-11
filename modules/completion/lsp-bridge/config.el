;;; completion/lsp-bridge/config.el -*- lexical-binding: t; -*-

(use-package! lsp-bridge
  :init
  (setq lsp-bridge-python-multi-lsp-server "pyright-langserver")
  (setq lsp-bridge-python-multi-lsp-server "pyright_ruff")
  :config
  (global-lsp-bridge-mode))
(after! lsp-bridge
  (map! :map evil-normal-state-map "C-]" 'lsp-bridge-find-def))
