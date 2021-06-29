;; -*- no-byte-compile: t; -*-
;;; lang/kotlin/packages.el

(package! kotlin-mode :pin "0e4bafb31d1fc2a0a420a521c2723d5526646c0b")

(when (featurep! :checkers syntax)
  (package! flycheck-kotlin :pin "bf1b398bdde128806a0a7479ebbe369bbaa40dae"))
