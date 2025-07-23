;;; completion/lsp-bridge/config.el -*- lexical-binding: t; -*-

(use-package! lsp-bridge
  :init
  (setq lsp-bridge-python-command "python")
  (setq lsp-bridge-python-multi-lsp-server "pyright-langserver")
  (setq lsp-bridge-python-multi-lsp-server "pyright_ruff")
  (setq acm-backend-lsp-enable-auto-import nil)
  :config
  (global-lsp-bridge-mode))
(after! lsp-bridge
  (map! :map lsp-bridge-mode-map "RET" 'newline-and-indent)
  (map! :map evil-normal-state-map "C-]" 'lsp-bridge-find-def))
