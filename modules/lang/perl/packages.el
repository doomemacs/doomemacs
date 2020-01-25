;; -*- no-byte-compile: t; -*-
;;; lang/perl/packages.el

(package! perl6-mode :pin "88de065795")

(when (featurep! :checkers syntax)
  (package! flycheck-perl6 :pin "b804702305"))
