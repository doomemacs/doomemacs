;;; lang/purescript/+psc-ide.el -*- lexical-binding: t; -*-
;;;###if (featurep! +psc-ide)

(use-package! psc-ide
  :hook (purescript-mode . psc-ide-mode)
  :config
  (remove-hook 'company-backends 'company-psc-ide-backend)
  (set-company-backend! 'purescript-mode 'company-psc-ide-backend))
