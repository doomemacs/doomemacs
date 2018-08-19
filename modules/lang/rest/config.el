;;; lang/rest/config.el -*- lexical-binding: t; -*-

(def-package! restclient
  :mode ("\\.http\\'" . restclient-mode)
  :config
  (set-popup-rule! "^\\*HTTP Response" :size 0.4 :quit 'other)

  ;; line numbers aren't enabled by default in fundamental-mode-derived modes
  (add-hook 'restclient-mode-hook #'doom|enable-line-numbers)

  ;; Forces underlying SSL verification to prompt for self-signed or invalid
  ;; certs, rather than silently reject them.
  (defun +rest*permit-self-signed-ssl (orig-fn &rest args)
    (let (gnutls-verify-error tls-checktrust)
      (apply orig-fn args)))
  (advice-add #'restclient-http-do :around #'+rest*permit-self-signed-ssl)

  (map! :mode restclient-mode
        :n [return] #'+rest/dwim-at-point
        :n "za" #'restclient-toggle-body-visibility
        :n "zm" #'+rest/fold-all
        :n "zr" #'outline-show-all

        :localleader
        :desc "Execute HTTP request"     :n "e" #'restclient-http-send-current
        :desc "Execute raw HTTP request" :n "E" #'restclient-http-send-current-raw
        :desc "Copy curl command"        :n "c" #'restclient-copy-curl-command))


(def-package! company-restclient
  :when (featurep! :completion company)
  :after restclient
  :config (set-company-backend! 'restclient-mode 'company-restclient))
