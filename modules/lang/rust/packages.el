;; -*- no-byte-compile: t; -*-
;;; lang/rust/packages.el

(package! rustic :pin "6ca73bb3cce4d1db3c4f91efb83b63227eb712d1")
(unless (featurep! +lsp)
  (package! racer :pin "1e63e98626737ea9b662d4a9b1ffd6842b1c648c"))
