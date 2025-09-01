;; -*- no-byte-compile: t; -*-
;;; lang/plantuml/packages.el

(package! plantuml-mode :pin "348e83ff193051d5ad332642100dd704f6e2a6d2")
(when (modulep! :checkers syntax)
  (package! flycheck-plantuml :pin "183be89e1dbba0b38237dd198dff600e0790309d"))

;; ob-plantuml is provided by org-plus-contrib
