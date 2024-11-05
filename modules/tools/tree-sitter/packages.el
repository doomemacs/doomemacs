;; -*- no-byte-compile: t; -*-
;;; tools/tree-sitter/packages.el

(package! tree-sitter :recipe (:branch "master") :pin "02fe7b86d92b1aab954045146469b7893f0ab371")
(package! tree-sitter-langs :pin "465a64032d45f51347a9b68a0e8f33d658039172")
(package! tree-sitter-indent :pin "4ef246db3e4ff99f672fe5e4b416c890f885c09e")

(when (modulep! :editor evil +everywhere)
  (package! evil-textobj-tree-sitter
    :pin "5056ebc231492827c159c878be30b55a42ae68be"))
