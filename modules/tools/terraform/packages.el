;; -*- no-byte-compile: t; -*-
;;; tools/terraform/packages.el

(package! terraform-mode)
(when (featurep! :completion company)
  (package! company-terraform))
