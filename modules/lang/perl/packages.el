;; -*- no-byte-compile: t; -*-
;;; lang/perl/packages.el

(package! perl6-mode :pin "88de065795d6863b23b6042576b9e90f8cbf8798")

(when (featurep! :checkers syntax)
  (package! flycheck-perl6 :pin "b804702305d7a6e26f762ff98cfdeec2e9dd4cb7"))
