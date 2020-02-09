;; -*- no-byte-compile: t; -*-
;;; lang/rust/packages.el

(package! rustic :pin "da3820de18")
(unless (featurep! +lsp)
  (package! racer :pin "a0bdf778f0"))
