;; -*- no-byte-compile: t; -*-
;;; lang/dart/packages.el

(package! dart-mode :pin "6229941ec5df40690142301cf7f3dd2e96ee7b91")

(when (and (modulep! +lsp)
           (modulep! :tools lsp -eglot))
  (package! lsp-dart :pin "2170823139269b77c39e3bf7600ff6c751a73b0d"))

(when (modulep! +flutter)
  (package! flutter :pin "e71235d400787d977da7ed792709437899c2a03c")
  (package! hover :pin "4ca0638a14a8b304ac2b46e7b342b8d8732ad199"))
