;; -*- no-byte-compile: t; -*-
;;; lang/rust/packages.el

(package! rustic :pin "f4b5c288af2a9833a104bc54850ddabe3996b8be")
(unless (featurep! +lsp)
  (package! racer :pin "1e63e98626737ea9b662d4a9b1ffd6842b1c648c"))
