(use-package swift-mode
  :mode "\\.swift$"
  :config
  (progn
    (after "flycheck" (add-to-list 'flycheck-cehckers 'swift))
    (after "company" (company--backend-on 'swift-mode-hook 'company-xcode))))

;; TODO Set up emacs task runners for fruitstrap

(provide 'init-swift)
;;; init-swift.el ends here
