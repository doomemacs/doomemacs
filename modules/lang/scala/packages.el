;; -*- no-byte-compile: t; -*-
;;; lang/scala/packages.el

(package! sbt-mode)
(package! scala-mode)

(unless (featurep! +lsp)
  (package! ensime))
