;; -*- no-byte-compile: t; -*-
;;; lang/rust/packages.el

(package! rustic)
(unless (featurep! +lsp)
  (package! racer))
