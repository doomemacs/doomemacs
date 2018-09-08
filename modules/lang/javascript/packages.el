;; -*- no-byte-compile: t; -*-
;;; lang/javascript/packages.el

(package! coffee-mode)
(package! eslintd-fix)
(package! js2-mode)
(package! js2-refactor)
(package! nodejs-repl)
(package! rjsx-mode)
(package! skewer-mode)
(package! tide)
(package! typescript-mode)
(package! npm-mode)

(when (featurep! :feature lookup)
  (package! xref-js2))

