;; -*- no-byte-compile: t; -*-
;;; tools/terraform/packages.el

(package! terraform-mode :pin "6973d1acab")
(when (featurep! :completion company)
  (package! company-terraform :pin "2d11a21fee"))
