;; -*- no-byte-compile: t; -*-
;;; lang/dart/packages.el

(package! dart-mode :pin "3bac14200f9f8f8fcebc383087572da5c3823c34")

(when (featurep! +lsp)
  (package! lsp-dart :pin "64fb5d93038483abab59751200749ad81698a845"))

(when (featurep! +flutter)
  (package! flutter :pin "81c524a43c46f4949ccde3b57e2a6ea359f712f4")
  (package! hover :pin "d0f03552c30e31193d3dcce7e927ce24b207cbf6"))
