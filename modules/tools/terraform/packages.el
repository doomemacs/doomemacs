;; -*- no-byte-compile: t; -*-
;;; tools/terraform/packages.el

(package! terraform-mode :pin "39d2fd5bfc86c6bf1c7bc38e6f0016d714f2d79d")
(when (modulep! :completion company)
  (package! company-terraform :pin "8d5a16d1bbeeb18ca49a8fd57b5d8cd30c8b8dc7"))
