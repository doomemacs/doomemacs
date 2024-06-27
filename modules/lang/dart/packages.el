;; -*- no-byte-compile: t; -*-
;;; lang/dart/packages.el

(package! dart-mode :pin "44beb628e5b4774062f7486008e06ae663e1ced0")

(when (and (modulep! +lsp)
           (not (modulep! :tools lsp +eglot)))
  (package! lsp-dart :pin "1f52e81c9371055ff9188117ace81f009d1c79f2"))

(when (modulep! +flutter)
  (package! flutter :pin "004c91e070a9b4a2a5042f5bb20015ec65453acf")
  (package! hover :pin "4ca0638a14a8b304ac2b46e7b342b8d8732ad199"))
