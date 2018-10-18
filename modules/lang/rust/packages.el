;; -*- no-byte-compile: t; -*-
;;; lang/rust/packages.el

;; requires rust cargo racer

(package! racer)
(package! rust-mode)

(when (featurep! :feature syntax-checker)
  (package! flycheck-rust))
