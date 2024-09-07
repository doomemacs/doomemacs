;; -*- no-byte-compile: t; -*-
;;; tools/tree-sitter/packages.el

(package! tree-sitter :recipe (:branch "master") :pin "02fe7b86d92b1aab954045146469b7893f0ab371")
(package! tree-sitter-langs :pin "365a4f7bf5184d04b5cc48175d93d7af7b8bbeb4")
(package! tree-sitter-indent :pin "4ef246db3e4ff99f672fe5e4b416c890f885c09e")

(when (modulep! :editor evil +everywhere)
  (package! evil-textobj-tree-sitter
    :pin "b4ef204ff80ed00b03cf8839ee29101ed867dd58"))
