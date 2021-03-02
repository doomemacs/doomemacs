;; -*- no-byte-compile: t; -*-
;;; tools/terraform/packages.el

(package! terraform-mode :pin "a9fa5bdaf58e9cae32ee44b7d0883f5600441b05")
(when (featurep! :completion company)
  (package! company-terraform :pin "2d11a21fee2f298e48968e479ddcaeda4d736e12"))
