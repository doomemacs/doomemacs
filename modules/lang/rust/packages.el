;; -*- no-byte-compile: t; -*-
;;; lang/rust/packages.el

(package! rustic :pin "13ba8cd0c7f99dcc7baa55b0d5c68bc03ac5b9e0")
(unless (featurep! +lsp)
  (package! racer :pin "1e63e98626737ea9b662d4a9b1ffd6842b1c648c"))
