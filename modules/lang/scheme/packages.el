;; -*- no-byte-compile: t; -*-
;;; lang/scheme/packages.el

(package! geiser :pin "adc5c4ab5ff33cf94cb3fcd892bb9503b5fa2aa2")

(when (featurep! :checkers syntax)
  (package! flycheck-guile
    :recipe (:host github :repo "flatwhatson/flycheck-guile")
    :pin "2940f1622fa352e7ca95a9e4ad65958c5575da02"))
