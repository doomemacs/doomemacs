;;; module-nim.el

(use-package nim-mode
  :mode "\\.nim$"
  :init
  (add-hook! nim-mode '(narf|enable-tab-width-2 flycheck-mode))
  :config
  (require 'flycheck-nim)
  (require 'company-nim)
  (define-company-backend! nim-mode (nim yasnippet))
  (map! :map nim-mode-map "gd" 'nim-goto-sym))

(provide 'module-nim)
;;; module-nim.el ends here
