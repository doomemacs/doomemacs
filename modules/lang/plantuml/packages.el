;; -*- no-byte-compile: t; -*-
;;; lang/plantuml/packages.el

(package! plantuml-mode :pin "ea45a13707")
(when (featurep! :checkers syntax)
  (package! flycheck-plantuml :pin "183be89e1d"))

;; ob-plantuml is provided by org-plus-contrib
