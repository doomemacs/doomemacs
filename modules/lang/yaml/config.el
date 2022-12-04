;;; lang/yaml/config.el -*- lexical-binding: t; -*-

(use-package! yaml-mode
  :mode "Procfile\\'"
  :init
  (when (modulep! +lsp)
    (add-hook 'yaml-mode-local-vars-hook #'lsp! 'append))
  (when (modulep! +tree-sitter)
    (add-hook 'yaml-mode-local-vars-hook #'tree-sitter! 'append))
  :config
  (setq-hook! 'yaml-mode-hook tab-width yaml-indent-offset))
