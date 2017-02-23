;; -*- no-byte-compile: t; -*-
;;; lang/javascript/packages.el

(package! coffee-mode)
(package! js2-mode)
(package! js2-refactor)
(package! jsx-mode)
(package! nodejs-repl)
(package! tern)

(when (featurep! :completion company)
  (package! company-tern))

