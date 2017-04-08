;; -*- no-byte-compile: t; -*-
;;; app/email/packages.el

(package! mu4e-alert)
(package! mu4e-maildirs-extension)

(when (featurep! :feature evil)
  (package! evil-mu4e))
