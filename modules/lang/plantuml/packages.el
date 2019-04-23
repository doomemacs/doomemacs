;; -*- no-byte-compile: t; -*-
;;; lang/plantuml/packages.el

(package! plantuml-mode)
(when (featurep! :tools flycheck)
  (package! flycheck-plantuml))
