;; -*- no-byte-compile: t; -*-
;;; lang/rust/packages.el

(package! rustic :pin "7c0e113d27ba414499f16c1667f58b28b35ffd0c")
(unless (featurep! +lsp)
  (package! racer :pin "a0bdf778f01e8c4b8a92591447257422ac0b455b"))
