;; -*- no-byte-compile: t; -*-
;;; lang/rust/packages.el

(package! rustic :pin "319e85515918ad8cc5348d945ebdf2a1718e1a64")
(unless (featurep! +lsp)
  (package! racer :pin "a0bdf778f01e8c4b8a92591447257422ac0b455b"))
