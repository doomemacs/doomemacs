;; -*- no-byte-compile: t; -*-
;;; lang/scala/packages.el

(package! sbt-mode :pin "7b121fce50430a44bd743583c87d7645bf157be2")
(package! scala-mode :pin "9d3b56e4877284a815b599a37d2b749d4f972226")

(when (featurep! +lsp)
  (package! lsp-metals :pin "31dafff1ebcddb860567e9b0681731adf25b54e6"))
