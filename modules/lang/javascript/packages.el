;; -*- no-byte-compile: t; -*-
;;; lang/javascript/packages.el

;; major modes
(package! coffee-mode)
(package! js2-mode)
(package! rjsx-mode)
(package! typescript-mode)

;; tools
(package! eslintd-fix)
(package! js2-refactor)
(package! nodejs-repl)
(package! npm-mode)
(package! skewer-mode)

(when (featurep! :feature lookup)
  (package! xref-js2))

(unless (featurep! +lsp)
  (package! tide))
