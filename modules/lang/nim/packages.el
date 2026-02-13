;; -*- no-byte-compile: t; -*-
;;; lang/nim/packages.el

;;; requires nim nimsuggest nimble

(package! nim-mode :pin "4502f83fbbd262a8a7e014db9affcbf05c8a1e79")

(when (modulep! :checkers syntax -flymake)
  (package! flycheck-nim :pin "ddfade51001571c2399f78bcc509e0aa8eb752a4"))
