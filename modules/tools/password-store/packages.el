;; -*- no-byte-compile: t; -*-
;;; tools/password-store/packages.el

(package! pass)
(package! password-store)

(when (version< emacs-version "26")
  (package! auth-password-store))

(when (featurep! :completion helm)
  (package! helm-pass))
