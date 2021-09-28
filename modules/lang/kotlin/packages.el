;; -*- no-byte-compile: t; -*-
;;; lang/kotlin/packages.el

(package! kotlin-mode :pin "876cc27dc105979a0b59782141785f8e172891e8")

(when (featurep! :checkers syntax)
  (package! flycheck-kotlin :pin "bf1b398bdde128806a0a7479ebbe369bbaa40dae"))
