;; -*- no-byte-compile: t; -*-
;;; lang/javascript/packages.el

;; requires node npm tern js-beautify eslint eslint-plugin-react

(package! coffee-mode)
(package! js2-mode)
(package! js2-refactor)
(package! rjsx-mode)
(package! nodejs-repl)
(package! web-beautify)
(package! tide)
(package! typescript-mode)
(package! skewer-mode)
(package! eslintd-fix)

(when (featurep! :feature lookup)
  (package! xref-js2))

