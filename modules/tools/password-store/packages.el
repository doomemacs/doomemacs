;; -*- no-byte-compile: t; -*-
;;; tools/password-store/packages.el

(package! password-store)
(package! pass)

(when (featurep! :completion helm)
  (package! helm-pass))
