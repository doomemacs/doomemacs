;; -*- no-byte-compile: t; -*-
;;; lang/rust/packages.el

(package! rustic :pin "d97ec8623c4c7e7ad3bb32e3d3773ba29a34bb0d")
(unless (featurep! +lsp)
  (package! racer :pin "1e63e98626737ea9b662d4a9b1ffd6842b1c648c"))
