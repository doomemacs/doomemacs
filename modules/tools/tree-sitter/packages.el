;; -*- no-byte-compile: t; -*-
;;; tools/tree-sitter/packages.el

(package! tree-sitter
  :pin "3cfab8a0e945db9b3df84437f27945746a43cc71")

(package! tree-sitter-langs
  :pin "deb2d8674be8f777ace50e15c7c041aeddb1d0b2")

(when (featurep! :editor evil +everywhere)
  (package! evil-textobj-tree-sitter
    :pin "0bf5bbbfecba95d49d40441ea54c6130e52bbeb1"))
