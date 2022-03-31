;; -*- no-byte-compile: t; -*-
;;; lang/kotlin/packages.el

(package! kotlin-mode :pin "3e0c34087ba4965a8bf08d3f27325f0a1e631bfb")

(when (featurep! :checkers syntax)
  (package! flycheck-kotlin :pin "bf1b398bdde128806a0a7479ebbe369bbaa40dae"))
