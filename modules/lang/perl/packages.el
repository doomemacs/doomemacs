;; -*- no-byte-compile: t; -*-
;;; lang/perl/packages.el

(package! perl6-mode)

(when (featurep! :checkers syntax)
  (package! flycheck-perl6))
