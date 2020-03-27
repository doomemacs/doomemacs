;; -*- no-byte-compile: t; -*-
;;; lang/javascript/packages.el

;; Major modes
(package! coffee-mode :pin "35a41c7d82")
(package! js2-mode :pin "fe53814dc2")
(package! rjsx-mode :pin "0061587a06")
(package! typescript-mode :pin "102587e458")

;; Tools
(package! eslintd-fix :pin "0c431141be")
(package! js2-refactor :pin "d4c40b5fc8")
(package! npm-mode :pin "3ee7c0bad5")

;; Eval
(package! nodejs-repl :pin "6fad7d764f")
(package! skewer-mode :pin "e5bed35193")

;; Programming environment
(package! tide :pin "3b45610faa")
(when (featurep! :tools lookup)
  (package! xref-js2 :pin "6f1ed5dae0"))
