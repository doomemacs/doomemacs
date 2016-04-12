;;; module-apple.el

(use-package applescript-mode :mode "\\.applescript$")


;;
;; LaunchBar: https://www.obdev.at/products/launchbar
;;

(define-minor-mode lb6-mode
  "Launchbar development mode."
  :init-value nil
  :lighter    " lb6"
  (add-yas-minor-mode! 'lb6-mode))
(define-builder! lb6-mode narf-lb6-reload)
(associate! lb6-mode :match "\\.lb\\(action\\|ext\\)/.+$")

(defun narf-lb6-reload ()
  (interactive)
  (let ((dir (f-traverse-upwards (lambda (f) (string-suffix-p ".lbaction" f)))))
    (shell-command (format "open '%s'" dir))))


;;
;; Swift
;;

;; TODO Set up emacs task runners for fruitstrap
(use-package swift-mode
  :mode "\\.swift$"
  :init (add-hook 'swift-mode-hook 'flycheck-mode)
  :config
  (after! flycheck (push 'swift flycheck-checkers))

  (require 'company-sourcekit)
  (define-company-backend! swift-mode (sourcekit yasnippet)))

(provide 'module-apple)
;;; module-apple.el ends here
