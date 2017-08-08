;;; lang/haskell/+intero.el -*- lexical-binding: t; -*-

(def-package! intero
  :commands intero-mode
  :init
  (add-hook 'haskell-mode-hook #'intero-mode)
  :config
  (unless (executable-find "stack")
    (warn "haskell-mode: couldn't find stack, disabling intero")
    (remove-hook 'haskell-mode-hook #'intero-mode))

  (add-hook! 'intero-mode-hook #'(flycheck-mode eldoc-mode))

  (set! :popup "^intero:backend:" :regex t :size 12)
  (set! :jump :definition #'intero-goto-definition))


(def-package! hindent
  :commands hindent-mode
  :init
  (add-hook 'haskell-mode-hook #'hindent-mode))
