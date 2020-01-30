;; -*- no-byte-compile: t; -*-
;;; lang/rust/packages.el

(package! rustic :pin "a6b8cd8db8")
(unless (featurep! +lsp)
  (package! racer :pin "a0bdf778f0"))
