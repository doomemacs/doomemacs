;; -*- no-byte-compile: t; -*-
;;; lang/rust/packages.el

(package! rustic :pin "32a962ab2d3f87bde0e12c4e8975fe73d8ba8579")
(unless (featurep! +lsp)
  (package! racer :pin "a0bdf778f01e8c4b8a92591447257422ac0b455b"))
