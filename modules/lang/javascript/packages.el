;; -*- no-byte-compile: t; -*-
;;; lang/javascript/packages.el

;; Major modes
(package! coffee-mode :pin "86ab8aae86")
(package! js2-mode :pin "b3841a7a30")
(package! rjsx-mode :pin "014c760138")
(package! typescript-mode :pin "a0f2c3ebd4")

;; Tools
(package! eslintd-fix :pin "98c669e365")
(package! js2-refactor :pin "d4c40b5fc8")
(package! npm-mode :pin "3ee7c0bad5")

;; Eval
(package! nodejs-repl :pin "8b90948265")
(package! skewer-mode :pin "123215dd9b")

;; Programming environment
(package! tide :pin "1878a097fc")
(when (featurep! :tools lookup)
  (package! xref-js2 :pin "6f1ed5dae0"))
