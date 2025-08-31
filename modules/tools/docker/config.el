;;; tools/docker/config.el -*- lexical-binding: t; -*-

(after! dockerfile-mode
  (set-docsets! 'dockerfile-mode "Docker")
  (set-formatter! 'dockfmt '("dockfmt" "fmt" filepath) :modes '(dockerfile-mode))

  (when (modulep! +lsp)
    (add-hook 'dockerfile-mode-local-vars-hook #'lsp! 'append)))


(use-package! dockerfile-ts-mode
  :when (modulep! +tree-sitter)
  :defer t
  :init
  (set-tree-sitter! 'dockerfile-mode 'dockerfile-ts-mode
    '((dockerfile :url "https://github.com/camdencheek/tree-sitter-dockerfile"
                  :commit "087daa20438a6cc01fa5e6fe6906d77c869d19fe"))))
