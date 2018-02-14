;;; lang/haskell/+intero.el -*- lexical-binding: t; -*-
;;;###if (featurep! +intero)

(def-package! intero
  :hook (haskell-mode . intero-mode)
  :config
  (unless (executable-find "stack")
    (warn! "Couldn't find stack. Intero has been disabled."))

  (add-hook! 'intero-mode-hook #'(flycheck-mode eldoc-mode))

  (set! :lookup 'haskell-mode :definition #'intero-goto-definition))


(def-package! hindent
  :hook (haskell-mode . hindent-mode))
