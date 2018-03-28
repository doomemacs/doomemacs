;; -*- no-byte-compile: t; -*-
;;; tools/password-store/packages.el

(package! pass)
(package! password-store)

(when (featurep! +auth)
  (package! auth-source-pass))

(when (featurep! :completion helm)
  (package! helm-pass))
