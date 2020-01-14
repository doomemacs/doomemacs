;; -*- no-byte-compile: t; -*-
;;; lang/kotlin/packages.el

(package! kotlin-mode)

(when (featurep! :checkers syntax)
  (package! flycheck-kotlin))
