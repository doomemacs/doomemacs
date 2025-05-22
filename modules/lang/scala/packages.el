;; -*- no-byte-compile: t; -*-
;;; lang/scala/packages.el

(package! sbt-mode :pin "cc68728a6ef0600aad369157b3a2d0ce56afba9b")
(package! scala-mode :pin "661337d8aa0a0cb418184c83757661603de3b2e3")

(when (and (modulep! +tree-sitter)
           (fboundp 'treesit-available-p))
  (package! scala-ts-mode :pin "c7671e10419261ef70b1820d3b970ad39f6fcfe2"))

(when (and (modulep! +lsp)
           (modulep! :tools lsp -eglot))
  (package! lsp-metals :pin "e1d9d04f3bab7e6e74916054b36ab1a87e831367"))
