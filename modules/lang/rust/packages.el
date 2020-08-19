;; -*- no-byte-compile: t; -*-
;;; lang/rust/packages.el

(package! rustic :pin "75b99201bb4e7a0bd990c006896ad7897f284ca2")
(unless (featurep! +lsp)
  (package! racer :pin "a0bdf778f01e8c4b8a92591447257422ac0b455b"))
