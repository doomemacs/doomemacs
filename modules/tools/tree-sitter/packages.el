;; -*- no-byte-compile: t; -*-
;;; tools/tree-sitter/packages.el

(package! tree-sitter
  :pin "588170ffbdbf02175cdb97547f7efd7e933a5a5f")
(package! tree-sitter-langs
  :pin "81adcdc06a9700ecf9659171da275b1c9dcfdfb6")

(when (featurep! :editor evil +everywhere)
  (package! evil-textobj-tree-sitter
    :pin "fe4d738486c4a267d799057409748063e036736e"))
