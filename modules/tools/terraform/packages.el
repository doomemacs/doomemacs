;; -*- no-byte-compile: t; -*-
;;; tools/terraform/packages.el

(package! terraform-mode :pin "6973d1acaba2835dfdf174f5a5e27de6366002e1")
(when (featurep! :completion company)
  (package! company-terraform :pin "2d11a21fee2f298e48968e479ddcaeda4d736e12"))
