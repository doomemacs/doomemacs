;; -*- no-byte-compile: t; -*-
;;; lang/rust/packages.el

(package! rust-mode)
(package! cargo)

(when (featurep! :tools flycheck)
  (package! flycheck-rust))

(unless (featurep! +lsp)
  (package! racer))
