;;; module-rest.el

(use-package restclient
  :commands restclient-mode
  :mode ("\\.http$" . restclient-mode)
  :config (def-popup! "*HTTP Response*" :size 25))

(use-package company-restclient
  :after restclient)

(provide 'module-rest)
;;; module-rest.el ends here
