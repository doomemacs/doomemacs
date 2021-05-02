;; -*- no-byte-compile: t; -*-
;;; lang/rust/packages.el

(package! rustic :pin "ed68fd3bb410869e1a4ce3943b5913ea88d9b509")
(unless (featurep! +lsp)
  (package! racer :pin "1e63e98626737ea9b662d4a9b1ffd6842b1c648c"))
