;; -*- no-byte-compile: t; -*-
;;; lang/rust/packages.el

(package! rustic :pin "804ebfe0295a6bf37870e06f84a8d35f55c9f1a6")
(unless (featurep! +lsp)
  (package! racer :pin "1e63e98626737ea9b662d4a9b1ffd6842b1c648c"))
