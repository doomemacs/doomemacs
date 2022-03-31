;; -*- no-byte-compile: t; -*-
;;; lang/rust/packages.el

(package! rustic :pin "6eec9713876d698510ee3715698fd42c1571e307")
(unless (featurep! +lsp)
  (package! racer :pin "1e63e98626737ea9b662d4a9b1ffd6842b1c648c"))
