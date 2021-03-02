;; -*- no-byte-compile: t; -*-
;;; lang/plantuml/packages.el

(package! plantuml-mode :pin "ea45a13707abd2a70df183f1aec6447197fc9ccc")
(when (featurep! :checkers syntax)
  (package! flycheck-plantuml :pin "183be89e1dbba0b38237dd198dff600e0790309d"))

;; ob-plantuml is provided by org-plus-contrib
