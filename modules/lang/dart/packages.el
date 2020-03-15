;; -*- no-byte-compile: t; -*-
;;; lang/dart/packages.el

(package! dart-mode :pin "04fcd649f1")
(when (featurep! +flutter)
  (package! flutter :pin "ec92a4df84"))
