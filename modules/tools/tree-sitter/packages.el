;; -*- no-byte-compile: t; -*-
;;; tools/tree-sitter/packages.el

(package! tree-sitter
  :pin "2a9d951cd6faf8367a402210c406f0f64818aa24")

(package! tree-sitter-langs
  :pin "ffe9ab0c8ec9e37e70e31d296df3b85bcfc73c5e")

(when (modulep! :editor evil +everywhere)
  (package! evil-textobj-tree-sitter
    :pin "71bf11072840fc85f728c1084896d892be4d3c56"))
