;;; module-swift.el

(use-package swift-mode
  :mode "\\.swift$"
  :config
  (after! flycheck (add-to-list 'flycheck-checkers 'swift))
  (after! company  (define-company-backend! swift-mode (xcode))))

;; TODO Set up emacs task runners for fruitstrap

(provide 'module-swift)
;;; module-swift.el ends here
