;;; tools/docker/config.el -*- lexical-binding: t; -*-

(after! dockerfile-mode
  (set-docsets! 'dockerfile-mode "Docker")
  (set-formatter! 'dockfmt '("dockfmt" "fmt" filepath) :modes '(dockerfile-mode))

  (when (modulep! +lsp)
    (add-hook 'dockerfile-mode-local-vars-hook #'lsp! 'append)))


(use-package! dockerfile-ts-mode
  :when (modulep! +tree-sitter)
  :when (fboundp 'dockerfile-ts-mode) ; 29.1+ only
  :defer t
  :init
  (set-tree-sitter! 'dockerfile-mode 'dockerfile-ts-mode 'dockerfile))
