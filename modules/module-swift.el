;;; module-swift.el

;; TODO Set up emacs task runners for fruitstrap
(use-package swift-mode
  :mode "\\.swift$"
  :init (add-hook 'swift-mode-hook 'flycheck-mode)
  :config
  (def-company-backend! swift-mode (sourcekit yasnippet))
  (def-docset! swift-mode ("Swift"))
  (def-repl! swift-mode swift-mode-run-repl) ; TODO test this
  (after! flycheck (push 'swift flycheck-checkers)))

(use-package company-sourcekit
  :after swift-mode)

(provide 'module-swift)
;;; module-swift.el ends here
