;; -*- no-byte-compile: t; -*-
;;; lang/dart/packages.el

(package! dart-mode :pin "04fcd649f19d49390079fbf2920a10bf37f6a634")

(when (featurep! +lsp)
  (package! lsp-dart :pin "9036e4b3a7666162fd1a6f8ec339157c28f788f0"))

(when (featurep! +flutter)
  (package! flutter :pin "78b3c572584c95220a40934bd67fd5e3fb096f96")
  (package! hover :pin "079efb10c5ece23ba0eccbe762f8b4185eac5810"))
