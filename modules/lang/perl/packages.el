;; -*- no-byte-compile: t; -*-
;;; lang/perl/packages.el

(package! perl6-mode)

(when (featurep! :tools flycheck)
  (package! flycheck-perl6))
