;; -*- no-byte-compile: t; -*-
;;; lang/scala/packages.el

(package! sbt-mode :pin "633a315ad4")
(package! scala-mode :pin "46bb948345")

(when (featurep! +lsp)
  (package! lsp-metals :pin "3d4d4b7b14d6f1041f75ddb45f36ca4cf9a6d854"))
