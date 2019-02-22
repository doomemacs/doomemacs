;; -*- no-byte-compile: t; -*-
;;; lang/nim/packages.el

;;; requires nim nimsuggest nimble

(package! nim-mode)

(when (featurep! :tools flycheck)
  (package! flycheck-nim))
