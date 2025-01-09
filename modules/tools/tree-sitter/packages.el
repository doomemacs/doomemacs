;; -*- no-byte-compile: t; -*-
;;; tools/tree-sitter/packages.el

(package! tree-sitter :recipe (:branch "master") :pin "fe98d0cae7babfabd089849808d3d5aa7970ef63")
(package! tree-sitter-langs :pin "4f951dbb530d254a0eb223431a0e0d63b2a7399a")
(package! tree-sitter-indent :pin "4ef246db3e4ff99f672fe5e4b416c890f885c09e")

(when (modulep! :editor evil +everywhere)
  (package! evil-textobj-tree-sitter
    :pin "bce236e5d2cc2fa4eae7d284ffd19ad18d46349a"))
