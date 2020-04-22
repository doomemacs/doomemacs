;; -*- no-byte-compile: t; -*-
;;; lang/dart/packages.el

(package! dart-mode :pin "04fcd649f1")

(when (featurep! +lsp)
  (package! lsp-dart :pin "4cd73b77f4"))

(when (featurep! +flutter)
  (package! flutter :pin "293b7225b9")
  (package! hover :pin "6f9ed1a651"))
