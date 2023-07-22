;; -*- no-byte-compile: t; -*-
;;; tools/tree-sitter/packages.el

(package! tree-sitter :pin "3cfab8a0e945db9b3df84437f27945746a43cc71")
(package! tree-sitter-langs :pin "944a734b7c3a5922a7e9a2009a9165e2dc195b18")
(package! tree-sitter-indent :pin "4ef246db3e4ff99f672fe5e4b416c890f885c09e")

(when (modulep! :editor evil +everywhere)
  (package! evil-textobj-tree-sitter
    :pin "e8bb9d63deeb2953ff9600e1633de667b3d7673e"))
