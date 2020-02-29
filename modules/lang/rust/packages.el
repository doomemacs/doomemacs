;; -*- no-byte-compile: t; -*-
;;; lang/rust/packages.el

(package! rustic :pin "99396915c7")
(unless (featurep! +lsp)
  (package! racer :pin "a0bdf778f0"))
