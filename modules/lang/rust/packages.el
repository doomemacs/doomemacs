;; -*- no-byte-compile: t; -*-
;;; lang/rust/packages.el

(package! rustic :pin "124e0286dc")
(unless (featurep! +lsp)
  (package! racer :pin "a0bdf778f0"))
