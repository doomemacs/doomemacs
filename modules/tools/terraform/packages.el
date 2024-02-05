;; -*- no-byte-compile: t; -*-
;;; tools/terraform/packages.el

(package! terraform-mode :pin "e8b57df8c2a3d3171f3768f60eb84067f553289c")
(when (modulep! :completion company)
  (package! company-terraform :pin "8d5a16d1bbeeb18ca49a8fd57b5d8cd30c8b8dc7"))
