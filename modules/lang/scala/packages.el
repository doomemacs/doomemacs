;; -*- no-byte-compile: t; -*-
;;; lang/scala/packages.el

(package! sbt-mode)
(package! scala-mode)

(if (featurep! +lsp)
    (package! lsp-scala)
  (package! ensime))
