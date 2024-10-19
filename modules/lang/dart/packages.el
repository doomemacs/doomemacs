;; -*- no-byte-compile: t; -*-
;;; lang/dart/packages.el

(package! dart-mode :pin "44beb628e5b4774062f7486008e06ae663e1ced0")

(when (and (modulep! +lsp)
           (modulep! :tools lsp -eglot))
  (package! lsp-dart :pin "9ffbafb7dcea3ef3d9e29bafb51d5167f0585d2c"))

(when (modulep! +flutter)
  (package! flutter :pin "e71235d400787d977da7ed792709437899c2a03c")
  (package! hover :pin "4ca0638a14a8b304ac2b46e7b342b8d8732ad199"))
