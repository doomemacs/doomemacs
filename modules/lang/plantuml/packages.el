;; -*- no-byte-compile: t; -*-
;;; lang/plantuml/packages.el

(package! plantuml-mode)
(when (featurep! :checkers syntax)
  (package! flycheck-plantuml))
