;; -*- no-byte-compile: t; -*-
;;; tools/password-store/packages.el

(package! pass)
(package! password-store)

(unless (package-built-in-p 'auth-store-pass)
  (package! auth-password-store))

(when (featurep! :completion helm)
  (package! helm-pass))
