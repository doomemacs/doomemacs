;; -*- no-byte-compile: t; -*-
;;; tools/terraform/packages.el

(package! terraform-mode :pin "a645c32a8f0f0d04034262ae5fea330d5c7a33c6")
(when (modulep! :completion company)
  (package! company-terraform :pin "8d5a16d1bbeeb18ca49a8fd57b5d8cd30c8b8dc7"))
