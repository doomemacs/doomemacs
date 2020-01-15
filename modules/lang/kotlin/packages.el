;; -*- no-byte-compile: t; -*-
;;; lang/kotlin/packages.el

(package! kotlin-mode :pin "ab610996820b5cbdb032edbf8747661131603ab8")

(when (featurep! :checkers syntax)
  (package! flycheck-kotlin :pin "5104ee9a3fdb7f0a0a3d3bcfd8dd3c45a9929310"))
