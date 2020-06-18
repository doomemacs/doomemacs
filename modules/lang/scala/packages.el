;; -*- no-byte-compile: t; -*-
;;; lang/scala/packages.el

(package! sbt-mode :pin "633a315ad4")
(package! scala-mode :pin "46bb948345")

(when (featurep! +lsp)
  (package! lsp-metals :pin "5468b638cd81b7d9ce9edc652c281b28bd775c23"))
