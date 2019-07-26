;;; lang/purescript/config.el -*- lexical-binding: t; -*-

(after! purescript-mode
  (add-hook! 'purescript-mode-hook
             #'purescript-indentation-mode
             #'rainbow-delimiters-mode)
  (set-lookup-handlers! 'purescript-mode
    :definition #'psc-ide-goto-definition
    :documentation #'purescript-pursuit))


;; (use-package! flycheck-purescript
;;   :after purescript-mode
;;   :config
;;   (add-hook 'flycheck-mode-hook #'flycheck-purescript-setup))


(use-package! psc-ide
  :hook (purescript-mode . psc-ide-mode)
  :config
  (remove-hook 'company-backends 'company-psc-ide-backend)
  (set-company-backend! 'purescript-mode 'company-psc-ide-backend))
