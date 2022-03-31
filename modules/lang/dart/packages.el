;; -*- no-byte-compile: t; -*-
;;; lang/dart/packages.el

(package! dart-mode :pin "3bac14200f9f8f8fcebc383087572da5c3823c34")

(when (featurep! +lsp)
  (package! lsp-dart :pin "fda433671f38874f0ebe66c43c64fec14af3f492"))

(when (featurep! +flutter)
  (package! flutter :pin "08138f8c95488aaf315a1f5d52c33deb8d28672b")
  (package! hover :pin "4ca0638a14a8b304ac2b46e7b342b8d8732ad199"))
