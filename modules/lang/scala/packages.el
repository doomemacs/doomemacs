;; -*- no-byte-compile: t; -*-
;;; lang/scala/packages.el

(package! sbt-mode :pin "7b121fce50430a44bd743583c87d7645bf157be2")
(package! scala-mode :pin "402d6df56457e9c512c3d77c7bcb0d04c77715f1")

(when (featurep! +lsp)
  (package! lsp-metals :pin "c76eeb6b580fadf6a16357be8c22c197d22574fd"))
