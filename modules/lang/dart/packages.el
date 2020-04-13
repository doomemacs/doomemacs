;; -*- no-byte-compile: t; -*-
;;; lang/dart/packages.el

(package! dart-mode :pin "04fcd649f1")

(when (featurep! +lsp)
  (package! lsp-dart :pin "064d47bad3"))

(when (featurep! +flutter)
  (package! flutter :pin "ec92a4df84")
  (package! hover :pin "6f9ed1a651"))
