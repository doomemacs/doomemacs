;; -*- no-byte-compile: t; -*-
;;; lang/plantuml/packages.el

(package! plantuml-mode :pin "5889166b6cfe94a37532ea27fc8de13be2ebfd02")
(when (featurep! :checkers syntax)
  (package! flycheck-plantuml :pin "183be89e1dbba0b38237dd198dff600e0790309d"))

;; ob-plantuml is provided by org-plus-contrib
