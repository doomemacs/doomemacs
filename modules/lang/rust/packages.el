;; -*- no-byte-compile: t; -*-
;;; lang/rust/packages.el

(package! rustic :pin "f7a94c4f914f4037fb84d50e91b3de90f606efda")
(unless (featurep! +lsp)
  (package! racer :pin "a0bdf778f01e8c4b8a92591447257422ac0b455b"))
