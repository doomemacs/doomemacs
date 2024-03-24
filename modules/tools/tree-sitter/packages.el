;; -*- no-byte-compile: t; -*-
;;; tools/tree-sitter/packages.el

(package! tree-sitter :pin "3cfab8a0e945db9b3df84437f27945746a43cc71")
(package! tree-sitter-langs :pin "c56cb511045d50e30f78508c54494e025d6af2cb")
(package! tree-sitter-indent :pin "4ef246db3e4ff99f672fe5e4b416c890f885c09e")

(when (modulep! :editor evil +everywhere)
  (package! evil-textobj-tree-sitter
    :pin "a19ab9d89a00f4a04420f9b5d61b66f04fea5261"))
