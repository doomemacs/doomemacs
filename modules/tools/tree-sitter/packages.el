;; -*- no-byte-compile: t; -*-
;;; tools/tree-sitter/packages.el

(package! tree-sitter :pin "3cfab8a0e945db9b3df84437f27945746a43cc71")
(package! tree-sitter-langs :pin "5eb24557f542d5fa18e7baaf07969cf7f297bfcd")
(package! tree-sitter-indent :pin "4ef246db3e4ff99f672fe5e4b416c890f885c09e")

(when (modulep! :editor evil +everywhere)
  (package! evil-textobj-tree-sitter
    :pin "9a9edd42a2dca9dfd0bc6026d47f689fa117b90f"))
