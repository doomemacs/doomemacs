;; -*- no-byte-compile: t; -*-
;;; tools/terraform/packages.el

(package! terraform-mode :pin "2967e7bdc0")
(when (featurep! :completion company)
  (package! company-terraform :pin "2d11a21fee"))
