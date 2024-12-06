;; -*- no-byte-compile: t; -*-
;;; tools/terraform/packages.el

(package! terraform-mode :pin "abfc10f5e313c4bb99de136a14636e9bc6df74f6")
(when (modulep! :completion company)
  (package! company-terraform :pin "8d5a16d1bbeeb18ca49a8fd57b5d8cd30c8b8dc7"))

(when (modulep! +docs)
  (package! terraform-docs
    :recipe (:host github :repo "loispostula/terraform-docs.el" :branch "main")
    :pin "94a78999ec03e66ce49f7343cc8705354e195c3a"))
