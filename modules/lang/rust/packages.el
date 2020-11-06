;; -*- no-byte-compile: t; -*-
;;; lang/rust/packages.el

(package! rustic :pin "7c9d55bdd47e98d2ac12e13dcb12703e6ffe377f")
(unless (featurep! +lsp)
  (package! racer :pin "a0bdf778f01e8c4b8a92591447257422ac0b455b"))
