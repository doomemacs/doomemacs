;; -*- no-byte-compile: t; -*-
;;; lang/nim/packages.el

;;; requires nim nimsuggest nimble

(package! nim-mode :pin "744e076f0bea1e69fedcdddf442d62251dd0f1f489")

(when (featurep! :checkers syntax)
  (package! flycheck-nim :pin "ddfade51001571c2399f78bcc509e0aa8eb752a4"))
