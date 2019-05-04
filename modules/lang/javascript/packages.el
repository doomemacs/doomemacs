;; -*- no-byte-compile: t; -*-
;;; lang/javascript/packages.el

;; Major modes
(package! coffee-mode)
(package! js2-mode)
(package! rjsx-mode)
(package! typescript-mode)

;; Tools
(package! eslintd-fix)
(package! js2-refactor)
(package! npm-mode)

;; Eval
(package! nodejs-repl)
(package! skewer-mode)

;; Programming environment
(package! tide)
(when (featurep! :tools lookup)
  (package! xref-js2))
