;;; tools/docker/config.el -*- lexical-binding: t; -*-

(after! dockerfile-mode
  (set-docsets! 'dockerfile-mode "Docker")

  (when (featurep! +lsp)
    (add-hook 'dockerfile-mode-local-vars-hook #'lsp!)))
