;; -*- no-byte-compile: t; -*-
;;; feature/syntax-checker/packages.el

(package! flycheck)
(package! flycheck-pos-tip)

(when (featurep! :lang emacs-lisp)
  (package! flycheck-cask))

