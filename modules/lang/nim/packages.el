;; -*- no-byte-compile: t; -*-
;;; lang/nim/packages.el

;;; requires nim nimsuggest nimble

(package! nim-mode :pin "1338e5b0d5e111ad932efb77d3cad680cc3b86c9")

(when (and (modulep! :checkers syntax)
           (not (modulep! :checkers syntax +flymake)))
  (package! flycheck-nim :pin "ddfade51001571c2399f78bcc509e0aa8eb752a4"))
