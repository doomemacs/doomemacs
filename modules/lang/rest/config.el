;;; lang/rest/config.el -*- lexical-binding: t; -*-

(use-package! restclient
  :mode ("\\.http\\'" . restclient-mode)
  ;; line numbers aren't enabled by default in fundamental-mode-derived modes
  :hook (restclient-mode . display-line-numbers-mode)
  :config
  (set-popup-rule! "^\\*HTTP Response" :size 0.4 :quit 'other)

  (setq-hook! 'restclient-mode-hook
    imenu-generic-expression '((nil "^[A-Z]+\s+.+" 0)))

  (defadvice! +rest--permit-self-signed-ssl-a (fn &rest args)
    "Forces underlying SSL verification to prompt for self-signed or invalid
certs, rather than reject them silently."
    :around #'restclient-http-do
    (require 'gnutls)
    (let (gnutls-verify-error)
      (apply fn args)))

  (map! :map restclient-mode-map
        :n [return] #'+rest/dwim-at-point
        :n "za" #'restclient-toggle-body-visibility
        :n "zm" #'+rest/fold-all
        :n "zr" #'outline-show-all

        :localleader
        "e" #'restclient-http-send-current
        "E" #'restclient-http-send-current-raw
        "c" #'restclient-copy-curl-command))


(use-package! company-restclient
  :when (modulep! :completion company)
  :after restclient
  :config (set-company-backend! 'restclient-mode 'company-restclient))


(use-package! restclient-jq
  :when (modulep! +jq)
  :after restclient)


(use-package! jq-mode
  :when (modulep! +jq)
  :after restclient-jq)
