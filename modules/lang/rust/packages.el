;; -*- no-byte-compile: t; -*-
;;; lang/rust/packages.el

(@package racer)
(@package rust-mode)

(when (@featurep :feature syntax-checker)
  (@package flycheck-rust))
(when (@featurep :completion company)
  (@package company-racer))
