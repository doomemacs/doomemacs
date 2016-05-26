;;; module-haskell.el

(use-package haskell
  :mode (("\\.hs$" . haskell-mode)
         ("\\.ghci$" . ghci-script-mode)
         ("\\.cabal$" . haskell-cabal-mode))
  :interpreter (("runghc" . haskell-mode)
                ("runhaskell" . haskell-mode))
  :init (add-hook! haskell-mode '(interactive-haskell-mode flycheck-mode))
  :config
  (def-popup! "*debug:haskell*" :size 20)
  (def-repl! haskell-mode switch-to-haskell)
  (push ".hi" completion-ignored-extensions))

(use-package inf-haskell
  :commands (inferior-haskell-mode inf-haskell-mode switch-to-haskell)
  :config (map! :map inf-haskell-mode-map "ESC ESC" 'doom/popup-close))

(provide 'module-haskell)
;;; module-haskell.el ends here
