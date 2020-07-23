;; -*- no-byte-compile: t; -*-
;;; lang/dart/packages.el

(package! dart-mode :pin "04fcd649f19d49390079fbf2920a10bf37f6a634")

(when (featurep! +lsp)
  (package! lsp-dart :pin "a06fc74e4cb0a468b7166dd9a7072ef5e9bdcc34"))

(when (featurep! +flutter)
  (package! flutter :pin "78b3c572584c95220a40934bd67fd5e3fb096f96")
  (package! hover :pin "6f9ed1a6517e3a43ef2deafc2f86c70b2abce008"))
