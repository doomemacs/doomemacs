;; -*- no-byte-compile: t; -*-
;;; lang/rust/packages.el

(package! rustic :pin "373f5a1940")
(unless (featurep! +lsp)
  (package! racer :pin "a0bdf778f0"))
