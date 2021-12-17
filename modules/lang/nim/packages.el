;; -*- no-byte-compile: t; -*-
;;; lang/nim/packages.el

;;; requires nim nimsuggest nimble

(package! nim-mode :pin "744e076f0bea1c5ddc49f92397d9aa98ffa7eff8")

(when (featurep! :checkers syntax)
  (package! flycheck-nim :pin "ddfade51001571c2399f78bcc509e0aa8eb752a4"))
