;;; module-haskell.el

(@def-package haskell
  :mode (("\\.hs$" . haskell-mode)
         ("\\.ghci$" . ghci-script-mode)
         ("\\.cabal$" . haskell-cabal-mode))
  :interpreter (("runghc" . haskell-mode)
                ("runhaskell" . haskell-mode))
  :init
  (@add-hook haskell-mode '(interactive-haskell-mode flycheck-mode))
  :config
  (@set :popup "*debug:haskell*" :size 20)
  (@set :repl 'haskell-mode 'switch-to-haskell)
  (push ".hi" completion-ignored-extensions))


(@def-package inf-haskell ; part of haskell
  :commands (inferior-haskell-mode inf-haskell-mode switch-to-haskell)
  :config (@map :map inf-haskell-mode-map "ESC ESC" 'doom/popup-close))

