;; -*- no-byte-compile: t; -*-
;;; lang/rust/packages.el

(package! rustic :pin "52b632d161b64bdca3f35e35180af63b668ce9fb")
(unless (featurep! +lsp)
  (package! racer :pin "a0bdf778f01e8c4b8a92591447257422ac0b455b"))
