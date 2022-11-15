;; -*- no-byte-compile: t; -*-
;;; lang/dart/packages.el

(package! dart-mode :pin "ae032b9b30ebadfe1b8a48a4cf278417e506d100")

(when (modulep! +lsp)
  (package! lsp-dart :pin "cc6c51b1e7887736c08260dbbcd28215c019a67a"))

(when (modulep! +flutter)
  (package! flutter :pin "49506681cd2d80713af5a04a2d33b8e6d89e3b96")
  (package! hover :pin "2b826735bb8d3bcfced489a1e0fa21b10fbc967e"))
