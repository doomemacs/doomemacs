;; -*- no-byte-compile: t; -*-
;;; lang/rust/packages.el

(package! rustic :pin "32a962ab2d")
(unless (featurep! +lsp)
  (package! racer :pin "a0bdf778f0"))
