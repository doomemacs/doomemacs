;;; lang/haskell/+dante.el -*- lexical-binding: t; -*-
;;;###if (featurep! +dante)

(def-package! dante
  :after haskell-mode
  :hook (haskell-mode . dante-mode)
  :config
  (unless (executable-find "cabal")
    (warn! "Couldn't find cabal, haskell-mode may have issues"))

  (add-hook 'haskell-mode-hook #'interactive-haskell-mode)
  (add-hook 'dante-mode-hook #'flycheck-mode))

