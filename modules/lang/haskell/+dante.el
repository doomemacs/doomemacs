;;; lang/haskell/+dante.el -*- lexical-binding: t; -*-
;;;###if (featurep! +dante)

(def-package! dante
  :after haskell-mode
  :hook (haskell-mode . dante-mode)
  :config
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode)

  (unless (executable-find "cabal")
    (warn "haskell-mode: couldn't find cabal")
    (remove-hook 'haskell-mode-hook #'dante-mode))

  (add-hook 'dante-mode-hook #'flycheck-mode))

