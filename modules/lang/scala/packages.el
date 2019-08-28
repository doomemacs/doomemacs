;; -*- no-byte-compile: t; -*-
;;; lang/scala/packages.el

(package! sbt-mode)
(package! scala-mode)

(unless (featurep! +lsp)
  ;; Fix #1697: Ensime doesn't have a master branch and its MELPA recipe doesn't
  ;; specify a branch. Straight can't handle packages with non-standard primary
  ;; branches (at the time of writing), so we must specify it manually:
  (package! ensime :recipe (:branch "2.0")))
