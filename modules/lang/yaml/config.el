;;; lang/yaml/config.el -*- lexical-binding: t; -*-

(use-package! yaml-mode
  :mode "Procfile\\'"
  :config
  (when (modulep! +lsp)
    (add-hook 'yaml-mode-local-vars-hook #'lsp! 'append)))


(use-package! yaml-ts-mode  ; 29.1+ only
  :when (modulep! +tree-sitter)
  :defer t
  :init
  (set-tree-sitter! 'yaml-mode 'yaml-ts-mode
    `((yaml :url "https://github.com/tree-sitter-grammars/tree-sitter-yaml"
            :rev ,(if (< (treesit-library-abi-version) 15) "v0.7.2" "v0.7.0"))))
  :config
  (when (modulep! +lsp)
    (add-hook 'yaml-ts-mode-local-vars-hook #'lsp! 'append)))
