;;; lang/haskell/config.el -*- lexical-binding: t; -*-

(def-package! haskell-mode
  :mode "\\.hs$"
  :mode ("\\.ghci$" . ghci-script-mode)
  :mode ("\\.cabal$" . haskell-cabal-mode)
  :interpreter (("runghc" . haskell-mode)
                ("runhaskell" . haskell-mode))
  :config
  (load "haskell-mode-autoloads" nil t)

  (set! :repl 'haskell-mode #'switch-to-haskell)
  (push ".hi" completion-ignored-extensions)

  (autoload 'switch-to-haskell "inf-haskell" nil t)
  (after! inf-haskell
    (map! :map inf-haskell-mode-map "ESC ESC" #'doom/popup-close)))


(def-package! dante
  :after haskell-mode
  :config
  (if (executable-find "cabal")
    (add-hook! 'haskell-mode-hook
      #'(flycheck-mode dante-mode interactive-haskell-mode))
    (warn "haskell-mode: couldn't find cabal")))


(def-package! company-ghc
  :when (featurep! :completion company)
  :after haskell-mode
  :config
  (set! :company-backend 'haskell-mode #'company-ghc)
  (setq company-ghc-show-info 'oneline)
  (if (executable-find "ghc-mod")
      (add-hook 'haskell-mode-hook #'ghc-comp-init)
    (warn "haskell-mode: couldn't find ghc-mode")))
