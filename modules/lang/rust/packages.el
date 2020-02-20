;; -*- no-byte-compile: t; -*-
;;; lang/rust/packages.el

(package! rustic :pin "6dead0cdd4")
(unless (featurep! +lsp)
  (package! racer :pin "a0bdf778f0"))
