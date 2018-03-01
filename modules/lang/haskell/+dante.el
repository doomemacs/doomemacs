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


(def-package! company-ghc
  :when (featurep! :completion company)
  :after haskell-mode
  :init
  (add-hook 'haskell-mode-hook #'ghc-comp-init)
  :config
  (unless (executable-find "ghc-mod")
    (warn! "Couldn't find ghc-mod on PATH. Code completion is disabled."))

  (setq company-ghc-show-info 'oneline)
  (set! :company-backend 'haskell-mode #'company-ghc))
