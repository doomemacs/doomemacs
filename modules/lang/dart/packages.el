;; -*- no-byte-compile: t; -*-
;;; lang/dart/packages.el

(package! dart-mode :pin "04fcd649f1")

(when (featurep! +lsp)
  (package! lsp-dart :pin "10b898ebeb60e00395bbc72495afaf2f22b4fc9f"))

(when (featurep! +flutter)
  (package! flutter :pin "293b7225b9")
  (package! hover :pin "6f9ed1a651"))
