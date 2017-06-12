;; -*- no-byte-compile: t; -*-
;;; tools/password-store/packages.el

(package! auth-password-store)
(package! pass)
(package! password-store)

(when (featurep! :completion helm)
  (package! helm-pass))
