;; -*- no-byte-compile: t; -*-
;;; lang/dart/packages.el

(package! dart-mode :pin "9c846769abd37f7fdc7ba8388d1f3a2b844b75e3")

(when (modulep! +lsp)
  (package! lsp-dart :pin "3db9f93c83052d6a8976c92d67d2b24473930760"))

(when (modulep! +flutter)
  (package! flutter :pin "edd3f5eb3f4603142f45c5890ee70b0dfb10772b")
  (package! hover :pin "4ca0638a14a8b304ac2b46e7b342b8d8732ad199"))
