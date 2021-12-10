;; -*- no-byte-compile: t; -*-
;;; tools/tree-sitter/packages.el

(package! tree-sitter
  :pin "8bbbfa4fc5f478f10c7cb968177d5a907fe5928f")

(package! tree-sitter-langs
  :pin "86a894a617976aefa453fa6ce8dd9871c58f733e")

(when (featurep! :editor evil +everywhere)
  (package! evil-textobj-tree-sitter
    :pin "4d79ea71219cb0153baf4046af8aae6b1ed2fcfb"))
