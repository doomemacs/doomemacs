;; -*- no-byte-compile: t; -*-
;;; lang/scala/packages.el

(package! sbt-mode :pin "cc68728a6ef0600aad369157b3a2d0ce56afba9b")
(package! scala-mode :pin "50bcafa181baec7054e27f4bca55d5f9277c6350")

(when (and (modulep! +tree-sitter) (treesit-available-p))
  (package! scala-ts-mode :pin "c7671e10419261ef70b1820d3b970ad39f6fcfe2"))

(when (and (modulep! +lsp)
           (modulep! :tools lsp -eglot))
  (package! lsp-metals :pin "e1d9d04f3bab7e6e74916054b36ab1a87e831367"))
