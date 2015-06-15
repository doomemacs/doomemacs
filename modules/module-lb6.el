;;; module-lb6.el

(define-minor-mode lb6-mode
  "Launchbar development mode."
  :init-value nil
  :lighter    "lb6"
  :keymap     (make-sparse-keymap)
  (add-yas-minor-mode! 'lb6-mode))

(associate! lb6-mode :match "\\.lb\\(action\\|ext\\)/.*$")

(provide 'module-lb6)
;;; module-lb6.el ends here
