;;; module-haskell.el

;; requires cabal (flycheck), ghci/hugs (repl)

(def-package! haskell-mode
  :mode (("\\.hs$" . haskell-mode)
         ("\\.ghci$" . ghci-script-mode)
         ("\\.cabal$" . haskell-cabal-mode))
  :interpreter (("runghc" . haskell-mode)
                ("runhaskell" . haskell-mode))
  :config
  (load "haskell-mode-autoloads" nil t)

  (set! :popup "*debug:haskell*" :size 20)
  (set! :repl 'haskell-mode 'switch-to-haskell)
  (push ".hi" completion-ignored-extensions)

  (autoload 'switch-to-haskell "inf-haskell" nil t)
  (after! inf-haskell
    (map! :map inf-haskell-mode-map "ESC ESC" 'doom/popup-close)))


(def-package! dante
  :after haskell-mode
  :config
  (when (executable-find "cabal")
    (add-hook! 'haskell-mode-hook
      '(flycheck-mode dante-mode interactive-haskell-mode))))

