;; -*- no-byte-compile: t; -*-
;;; tools/terraform/packages.el

(package! terraform-mode :pin "63fafc635b04b1b72e408e7dcb21c1fac78fc60b")
(when (featurep! :completion company)
  (package! company-terraform :pin "2d11a21fee2f298e48968e479ddcaeda4d736e12"))
