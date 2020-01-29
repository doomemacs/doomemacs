;; -*- no-byte-compile: t; -*-
;;; lang/rust/packages.el

(package! rustic :pin "c7cab3fbe6")
(unless (featurep! +lsp)
  (package! racer :pin "a0bdf778f0"))
