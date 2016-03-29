;;; module-haskell.el

(use-package haskell-mode
  :mode "\\.hs$"
  :config
  (define-repl! haskell-mode switch-to-haskell)

  ;; haskell-mode complains that this function isn't defined, and it isn't!
  (defun haskell-mode-after-save-handler ())

  (use-package inf-haskell
    :commands (switch-to-haskell)
    :init (evil-set-initial-state 'inferior-haskell-mode 'emacs)
    :config
    (map! :map inf-haskell-mode-map
          "ESC ESC" 'narf/popup-close)))

(provide 'module-haskell)
;;; module-haskell.el ends here
