;; -*- no-byte-compile: t; -*-
;;; lang/dart/packages.el

(package! dart-mode :pin "04fcd649f1")

;; Optional module features

(when (featurep! +flutter)
  (package! flutter :pin "ec92a4df84"))

(when (featurep! +lsp)
  (package! lsp-dart :pin "c66a7eb91c"))
