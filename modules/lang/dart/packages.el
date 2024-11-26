;; -*- no-byte-compile: t; -*-
;;; lang/dart/packages.el

(package! dart-mode :pin "02e919c1cf200b4938139c18068577faff1fb364")

(when (and (modulep! +lsp)
           (modulep! :tools lsp -eglot))
  (package! lsp-dart :pin "7e3d3429418bc42cda7fa7b58e6644a705cf2f89"))

(when (modulep! +flutter)
  (package! flutter :pin "e71235d400787d977da7ed792709437899c2a03c")
  (package! hover :pin "4ca0638a14a8b304ac2b46e7b342b8d8732ad199"))
