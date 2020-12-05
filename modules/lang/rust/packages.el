;; -*- no-byte-compile: t; -*-
;;; lang/rust/packages.el

(package! rustic :pin "91ad5db27b86bb2ba6f3019b764d0f45ec93f484")
(unless (featurep! +lsp)
  (package! racer :pin "a0bdf778f01e8c4b8a92591447257422ac0b455b"))
