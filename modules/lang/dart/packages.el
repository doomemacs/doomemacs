;; -*- no-byte-compile: t; -*-
;;; lang/dart/packages.el

(package! dart-mode :pin "43975c92080e307c4bc14a4773a61195d2062fd9")

(when (featurep! +lsp)
  (package! lsp-dart :pin "64fb5d93038483abab59751200749ad81698a845"))

(when (featurep! +flutter)
  (package! flutter :pin "960b63576a13b7bd3495d0ad1883ed736873543b")
  (package! hover :pin "c9c0593b2bffd6a494f570d707fe8d4e97718da4"))
