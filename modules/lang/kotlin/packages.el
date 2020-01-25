;; -*- no-byte-compile: t; -*-
;;; lang/kotlin/packages.el

(package! kotlin-mode :pin "ab61099682")

(when (featurep! :checkers syntax)
  (package! flycheck-kotlin :pin "5104ee9a3f"))
