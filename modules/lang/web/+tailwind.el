;;; lang/web/+tailwind.el -*- lexical-binding: t; -*-

(use-package! lsp-tailwindcss
  :when (and (featurep! +lsp) (featurep! +tailwind))
  :after lsp-mode
  :init
  (setq lsp-tailwindcss-add-on-mode t)
  :config
  (setq lsp-tailwindcss-major-modes '(web-mode css-mode rjsx-mode typescript-tsx-mode)
        lsp-tailwindcss-emmet-completions (featurep 'emmet-mode)))

(set-docsets! '(web-mode css-mode rjsx-mode typescript-tsx-mode)
  :add "Tailwind_CSS")
