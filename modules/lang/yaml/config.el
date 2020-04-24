;;; lang/yaml/config.el -*- lexical-binding: t; -*-

(use-package! yaml-mode
  :mode "Procfile\\'"
  :init
  (when (featurep! +lsp)
    (add-hook 'yaml-mode-local-vars-hook #'lsp!))
  :config
  (setq-hook! 'yaml-mode-hook tab-width yaml-indent-offset))
