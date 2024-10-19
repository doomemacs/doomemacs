;; -*- no-byte-compile: t; -*-
;;; lang/nim/packages.el

;;; requires nim nimsuggest nimble

(package! nim-mode :pin "625cc023bd75a741b7d4e629e5bec3a52f45b4be")

(when (modulep! :checkers syntax -flymake)
  (package! flycheck-nim :pin "ddfade51001571c2399f78bcc509e0aa8eb752a4"))
