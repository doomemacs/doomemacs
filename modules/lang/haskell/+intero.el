;;; lang/haskell/+intero.el -*- lexical-binding: t; -*-
;;;###if (featurep! +intero)

(def-package! intero
  :after haskell-mode
  :config
  (add-hook 'haskell-mode-hook #'+haskell|init-intero)
  (add-hook! 'intero-mode-hook #'(flycheck-mode eldoc-mode))
  (set! :lookup 'haskell-mode :definition #'intero-goto-definition))


(def-package! hindent
  :hook (haskell-mode . hindent-mode))
