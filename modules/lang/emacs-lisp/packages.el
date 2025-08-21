;; -*- no-byte-compile: t; -*-
;;; lang/emacs-lisp/packages.el

(package! elisp-mode :built-in t)

;; Fontification plugins
(package! highlight-quoted :pin "24103478158cd19fbcfb4339a3f1fa1f054f1469")

;; Tools
(package! helpful :pin "03756fa6ad4dcca5e0920622b1ee3f70abfc4e39")
(package! macrostep :pin "d0928626b4711dcf9f8f90439d23701118724199")
(package! overseer :pin "7fdcf1a6fba6b1569a09c1666b4e51bcde266ed9")
(package! elisp-def :pin "2451ed6594807448a24a63c51c917727713ed19d")
(package! elisp-demos :pin "1a108d1c5011f9ced58be2ca98bea1fbd4130a2f")
(when (modulep! :checkers syntax -flymake)
  (package! flycheck-package :pin "a52e4e95f3151898b36739dfdb4a98b368626fc0"))

;; Libraries
(package! buttercup :pin "cc5a2ab7c7f18aaaf525fac61fe59bae5ad018dd")
