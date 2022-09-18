;; -*- no-byte-compile: t; -*-
;;; tools/terraform/packages.el

(package! terraform-mode :pin "e560caaa9d9a11b0868adf6d9dcae5ebb5055730")
(when (modulep! :completion company)
  (package! company-terraform :pin "8d5a16d1bbeeb18ca49a8fd57b5d8cd30c8b8dc7"))
