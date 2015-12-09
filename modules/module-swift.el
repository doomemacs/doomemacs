;;; module-swift.el

(use-package swift-mode
  :mode "\\.swift$"
  :init
  (add-hook! swift-mode 'flycheck-mode)
  :config
  (after! flycheck (add-to-list 'flycheck-checkers 'swift))
  (after! company
    (require 'company-sourcekit)
    (define-company-backend! swift-mode (sourcekit yasnippet))))

;; TODO Set up emacs task runners for fruitstrap

(provide 'module-swift)
;;; module-swift.el ends here
