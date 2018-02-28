;;; lang/haskell/config.el -*- lexical-binding: t; -*-

(cond ((featurep! +intero) (load! +intero))
      ((featurep! +dante)  (load! +dante)))


;;
;; Common plugins
;;

(def-package! haskell-mode
  :mode "\\.hs$"
  :mode ("\\.ghci$" . ghci-script-mode)
  :mode ("\\.cabal$" . haskell-cabal-mode)
  :interpreter (("runghc" . haskell-mode)
                ("runhaskell" . haskell-mode))
  :config
  (set! :repl 'haskell-mode #'switch-to-haskell)
  (push ".hi" completion-ignored-extensions)

  (autoload 'switch-to-haskell "inf-haskell" nil t)
  (after! inf-haskell
    (map! :map inferior-haskell-mode-map "ESC ESC" #'doom/popup-close)))


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

