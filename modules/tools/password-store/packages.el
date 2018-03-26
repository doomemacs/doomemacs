;; -*- no-byte-compile: t; -*-
;;; tools/password-store/packages.el

(package! pass)
(package! password-store)
(package! auth-source-pass)

(when (featurep! :completion helm)
  (package! helm-pass))
