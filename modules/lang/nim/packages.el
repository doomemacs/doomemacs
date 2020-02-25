;; -*- no-byte-compile: t; -*-
;;; lang/nim/packages.el

;;; requires nim nimsuggest nimble

(package! nim-mode :pin "16a245e497")

(when (featurep! :checkers syntax)
  (package! flycheck-nim :pin "ddfade5100"))
