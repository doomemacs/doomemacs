;; -*- no-byte-compile: t; -*-
;;; lang/scala/packages.el

(package! sbt-mode :pin "633a315ad4")
(package! scala-mode :pin "46bb948345")

(when (featurep! +lsp)
  (package! lsp-metals :pin "0d2adbd000c0ef5bbd219fea3426eb1475779c9b"))
