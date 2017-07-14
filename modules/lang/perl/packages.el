;; -*- no-byte-compile: t; -*-
;;; lang/perl/packages.el

(package! perl6-mode)

(when (featurep! :feature syntax-checker)
  (package! flycheck-perl6))
