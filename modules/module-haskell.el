;;; module-haskell.el

(use-package haskell
  :mode (("\\.hs$" . haskell-mode)
         ("\\.ghci$" . ghci-script-mode)
         ("\\.cabal$" . haskell-cabal-mode))
  :init
  (define-repl! haskell-mode switch-to-haskell)
  (add-hook! haskell-mode '(interactive-haskell-mode flycheck-mode))
  :config
  ;; haskell-mode complains that this function isn't defined, and it isn't!
  (defun haskell-mode-after-save-handler ()))

(use-package inf-haskell
  :commands (inferior-haskell-mode inf-haskell-mode switch-to-haskell)
  :init (evil-set-initial-state 'inferior-haskell-mode 'emacs)
  :config
  (define-key inf-haskell-mode-map (kbd "ESC ESC") 'narf/popup-close))

(provide 'module-haskell)
;;; module-haskell.el ends here
