;;; lang/rest/config.el

(def-package! restclient
  :commands restclient-mode
  :mode ("\\.http$" . restclient-mode))


(def-package! company-restclient
  :when (featurep! :completion company)
  :after restclient
  :config (set! :company-backend 'restclient-mode '(company-restclient)))
