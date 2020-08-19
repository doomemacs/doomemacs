;; -*- no-byte-compile: t; -*-
;;; lang/kotlin/packages.el

(package! kotlin-mode :pin "8e6dd578f2b3d77ac33b6384d2bfe1b1f6799a1a")

(when (featurep! :checkers syntax)
  (package! flycheck-kotlin :pin "5104ee9a3fdb7f0a0a3d3bcfd8dd3c45a9929310"))
