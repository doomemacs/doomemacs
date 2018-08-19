;; -*- no-byte-compile: t; -*-
;;; lang/nim/packages.el

;;; requires nim nimsuggest nimble

(package! nim-mode)

(when (featurep! :feature syntax-checker)
  (package! flycheck-nim))
