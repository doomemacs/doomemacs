;; -*- no-byte-compile: t; -*-
;;; tools/terraform/packages.el

(package! terraform-mode :pin "e560caaa9d9a11b0868adf6d9dcae5ebb5055730")
(when (featurep! :completion company)
  (package! company-terraform :pin "2d11a21fee2f298e48968e479ddcaeda4d736e12"))
