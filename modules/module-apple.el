;;; module-apple.el

(use-package applescript-mode :mode "\\.applescript$")

;; TODO Set up emacs task runners for fruitstrap
(use-package swift-mode
  :mode "\\.swift$"
  :init
  (add-hook! swift-mode 'flycheck-mode)
  :config
  (after! flycheck (add-to-list 'flycheck-checkers 'swift))
  (after! company
    (require 'company-sourcekit)
    (define-company-backend! swift-mode (sourcekit yasnippet))))

(defun narf-lb6-reload ()
  (interactive)
  (let ((dir (f-traverse-upwards (lambda (f) (string-suffix-p ".lbaction" f)))))
    (shell-command (format "open '%s'" dir))))

(define-minor-mode lb6-mode
  "Launchbar development mode."
  :init-value nil
  :lighter    " lb6"
  (add-yas-minor-mode! 'lb6-mode))
(define-builder! lb6-mode narf-lb6-reload)
(associate! lb6-mode :match "\\.lb\\(action\\|ext\\)/.+$")

(provide 'module-apple)
;;; module-apple.el ends here
