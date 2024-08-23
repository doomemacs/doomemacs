;; -*- no-byte-compile: t; -*-
;;; tools/tree-sitter/packages.el

(package! tree-sitter :recipe (:branch "master") :pin "02fe7b86d92b1aab954045146469b7893f0ab371")
(package! tree-sitter-langs :pin "1c3d95d018c6ab09dbe0b31530bfb1865fac2e5f")
(package! tree-sitter-indent :pin "4ef246db3e4ff99f672fe5e4b416c890f885c09e")

(when (modulep! :editor evil +everywhere)
  (package! evil-textobj-tree-sitter
    :pin "041fcb9fbb00deac92a4e58b5fca03f7dc0a32e8"))
