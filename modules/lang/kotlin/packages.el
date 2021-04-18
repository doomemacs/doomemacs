;; -*- no-byte-compile: t; -*-
;;; lang/kotlin/packages.el

(package! kotlin-mode :pin "0e4bafb31d1fc2a0a420a521c2723d5526646c0b")

(when (featurep! :checkers syntax)
  (package! flycheck-kotlin :pin "5104ee9a3fdb7f0a0a3d3bcfd8dd3c45a9929310"))
