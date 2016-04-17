;;; module-apple.el

(use-package applescript-mode :mode "\\.applescript$")


;;
;; LaunchBar: https://www.obdev.at/products/launchbar
;;

(define-project-type! lb6 "lb6"
  :match "\\.lb\\(action\\|ext\\)/.+$"
  :build (lambda ()
           (awhen (f-traverse-upwards (lambda (f) (f-ext? f "lbaction")))
             (shell-command (format "open '%s'" it)))))


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
