;; -*- no-byte-compile: t; -*-
;;; lang/rust/packages.el

(package! rustic :pin "f7d5ac0c930ae435421f3f5bc827e8c61ce73662")
(unless (featurep! +lsp)
  (package! racer :pin "a0bdf778f01e8c4b8a92591447257422ac0b455b"))
