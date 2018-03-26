;; -*- no-byte-compile: t; -*-
;;; tools/password-store/packages.el

(package! pass)
(package! password-store)
(package! auth-source-store)

(when (featurep! :completion helm)
  (package! helm-pass))
