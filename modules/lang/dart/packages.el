;; -*- no-byte-compile: t; -*-
;;; lang/dart/packages.el

(package! dart-mode :pin "43975c92080e307c4bc14a4773a61195d2062fd9")

(when (featurep! +lsp)
  (package! lsp-dart :pin "f3b70ec0e6adf3a51e15f9a3effb182c2363493d"))

(when (featurep! +flutter)
  (package! flutter :pin "960b63576a13b7bd3495d0ad1883ed736873543b")
  (package! hover :pin "c9c0593b2bffd6a494f570d707fe8d4e97718da4"))
