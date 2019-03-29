;; -*- no-byte-compile: t; -*-
;;; lang/kotlin/packages.el

(package! kotlin-mode)

(when (featurep! :tools flycheck)
  (package! flycheck-kotlin))
