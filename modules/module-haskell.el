;;; module-haskell.el

(use-package haskell-mode
  :mode "\\.hs$"
  :config
  ;; haskell-mode complains that this function isn't defined, and it isn't!
  (defun haskell-mode-after-save-handler ()))

(provide 'module-haskell)
;;; module-haskell.el ends here
