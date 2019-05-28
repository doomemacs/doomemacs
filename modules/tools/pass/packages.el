;; -*- no-byte-compile: t; -*-
;;; tools/pass/packages.el

(package! pass)
(package! password-store)
(package! password-store-otp)
(package! auth-source-pass :pin "melpa")

(when (featurep! :completion ivy)
  (package! ivy-pass))
(when (featurep! :completion helm)
  (package! helm-pass))
