;; -*- no-byte-compile: t; -*-
;;; tools/terraform/packages.el

(package! terraform-mode :pin "5bdd734a87f67f6574664f63eb4d02a0bc74c0d0")
(when (modulep! :completion company)
  (package! company-terraform :pin "8d5a16d1bbeeb18ca49a8fd57b5d8cd30c8b8dc7"))
