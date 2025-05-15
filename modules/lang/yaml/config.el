;;; lang/yaml/config.el -*- lexical-binding: t; -*-

(use-package! yaml-mode
  :mode "Procfile\\'"
  :config
  (when (modulep! +lsp)
    (add-hook 'yaml-mode-local-vars-hook #'lsp! 'append))
  (setq-hook! 'yaml-mode-hook tab-width yaml-indent-offset))


(use-package! yaml-ts-mode
  :when (modulep! +tree-sitter)
  :when (fboundp 'yaml-ts-mode) ; 29.1+ only
  :init
  (set-tree-sitter! 'yaml-mode 'yaml-ts-mode
    '((yaml :url "https://github.com/tree-sitter-grammars/tree-sitter-yaml"
            :rev "v0.7.0")))
  :config
  ;; HACK: Rely on `major-mode-remap-defaults'.
  (cl-callf2 rassq-delete-all 'yaml-ts-mode auto-mode-alist)

  (when (modulep! +lsp)
    (add-hook 'yaml-ts-mode-local-vars-hook #'lsp! 'append)))
