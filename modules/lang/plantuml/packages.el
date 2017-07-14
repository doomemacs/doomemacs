;; -*- no-byte-compile: t; -*-
;;; lang/plantuml/packages.el

(package! plantuml-mode)
(when (featurep! :feature syntax-checker)
  (package! flycheck-plantuml))
