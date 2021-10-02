;;; lang/web/+tailwind.el -*- lexical-binding: t; -*-

(use-package! lsp-tailwindcss
  :when (and (featurep! +lsp) (featurep! +tailwind))
  :after lsp-mode
  :init
  (setq lsp-tailwindcss-add-on-mode t
        lsp-tailwindcss-emmet-completions (featurep 'emmet-mode))
  :config
  (add-to-list 'lsp-tailwindcss-major-modes 'typescript-tsx-mode))
