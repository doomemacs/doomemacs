(use-package swift-mode
  :mode "\\.swift$"
  :config
  (progn
    (after "flycheck" (add-to-list 'flycheck-cehckers 'swift))
    (after "company" (narf/add-company-backend swift-mode (company-xcode)))))

;; TODO Set up emacs task runners for fruitstrap

(provide 'init-swift)
;;; init-swift.el ends here
