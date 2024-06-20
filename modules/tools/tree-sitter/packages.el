;; -*- no-byte-compile: t; -*-
;;; tools/tree-sitter/packages.el

(package! tree-sitter :pin "02fe7b86d92b1aab954045146469b7893f0ab371")
(package! tree-sitter-langs :pin "a06804e13fc1d24d0099b44ce23d6ac5e6bac5b8")
(package! tree-sitter-indent :pin "4ef246db3e4ff99f672fe5e4b416c890f885c09e")

(when (modulep! :editor evil +everywhere)
  (package! evil-textobj-tree-sitter
    :pin "a19ab9d89a00f4a04420f9b5d61b66f04fea5261"))
