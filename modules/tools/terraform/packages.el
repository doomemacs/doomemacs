;; -*- no-byte-compile: t; -*-
;;; tools/terraform/packages.el

(package! terraform-mode :pin "2967e7bdc05d15617e121052f6e43c61439b9070")
(when (featurep! :completion company)
  (package! company-terraform :pin "2d11a21fee2f298e48968e479ddcaeda4d736e12"))
