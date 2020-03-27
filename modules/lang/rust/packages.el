;; -*- no-byte-compile: t; -*-
;;; lang/rust/packages.el

(package! rustic :pin "61032eacf0")
(unless (featurep! +lsp)
  (package! racer :pin "a0bdf778f0"))
