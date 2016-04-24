;;; module-haskell.el

(use-package haskell
  :mode (("\\.hs$" . haskell-mode)
         ("\\.ghci$" . ghci-script-mode)
         ("\\.cabal$" . haskell-cabal-mode))
  :interpreter (("runghc" . haskell-mode)
                ("runhaskell" . haskell-mode))
  :init
  (def-repl! haskell-mode switch-to-haskell)
  (add-hook! haskell-mode '(interactive-haskell-mode flycheck-mode))
  :config
  (push ".hi" completion-ignored-extensions))

(use-package inf-haskell
  :commands (inferior-haskell-mode inf-haskell-mode switch-to-haskell)
  :config
  (map! :map inf-haskell-mode-map "ESC ESC" 'narf/popup-close))

(provide 'module-haskell)
;;; module-haskell.el ends here
