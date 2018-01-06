;;; lang/rest/config.el -*- lexical-binding: t; -*-

(def-package! restclient
  :commands restclient-mode
  :mode ("\\.http$" . restclient-mode)
  :config
  (set! :popup "^\\*HTTP Response" '((size . 0.4)) '((quit . other)))
  (map! :mode restclient-mode
        :n [M-return] 'restclient-http-send-current
        :localleader
        :desc "Execute HTTP request"     :n "e" 'restclient-http-send-current
        :desc "Execute raw HTTP request" :n "E" 'restclient-http-send-current-raw
        :desc "Copy curl command"        :n "c" 'restclient-copy-curl-command))


(def-package! company-restclient
  :when (featurep! :completion company)
  :after restclient
  :config (set! :company-backend 'restclient-mode '(company-restclient)))
