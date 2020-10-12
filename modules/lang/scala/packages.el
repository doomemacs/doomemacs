;; -*- no-byte-compile: t; -*-
;;; lang/scala/packages.el

(package! sbt-mode :pin "4358ed862a1b1ec18ac9699afb4862146669eb79")
(package! scala-mode :pin "1d08e885b1489313666c7f15a3962432a4f757ee")

(when (featurep! +lsp)
  (package! lsp-metals :pin "8f8471ca1b2a3d54bafce8949e6b13c9567d0405"))
