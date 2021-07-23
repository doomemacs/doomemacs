;; -*- no-byte-compile: t; -*-
;;; lang/dart/packages.el

(package! dart-mode :pin "43975c92080e307c4bc14a4773a61195d2062fd9")

(when (featurep! +lsp)
  (package! lsp-dart :pin "01d89d43f17a15c7ccad5a458250d5d6b0f70b09"))

(when (featurep! +flutter)
  (package! flutter :pin "960b63576a13b7bd3495d0ad1883ed736873543b")
  (package! hover :pin "c9c0593b2bffd6a494f570d707fe8d4e97718da4"))
