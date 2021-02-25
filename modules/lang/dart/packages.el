;; -*- no-byte-compile: t; -*-
;;; lang/dart/packages.el

(package! dart-mode :pin "04fcd649f19d49390079fbf2920a10bf37f6a634")

(when (featurep! +lsp)
  (package! lsp-dart :pin "71902caafbb20edb672641e44eca4cdf173e8a4f"))

(when (featurep! +flutter)
  (package! flutter :pin "696228a619f6078b16f9f77071112f6ad2a25c4e")
  (package! hover :pin "c9c0593b2bffd6a494f570d707fe8d4e97718da4"))
