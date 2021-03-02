;; -*- no-byte-compile: t; -*-
;;; lang/rust/packages.el

(package! rustic :pin "61d600e5598a37034b8b539bd50966c3eb557f10")
(unless (featurep! +lsp)
  (package! racer :pin "f17f9d73c74ac86001a19d08735e6b966d6c5609"))
