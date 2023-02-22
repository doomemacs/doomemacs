;; -*- no-byte-compile: t; -*-
;;; tools/tree-sitter/packages.el

(package! tree-sitter :pin "c3fe96a103a766256ba62120eb638eef8e9a9802")
(package! tree-sitter-langs :pin "deb2d8674be8f777ace50e15c7c041aeddb1d0b2")
(package! tree-sitter-indent :pin "4ef246db3e4ff99f672fe5e4b416c890f885c09e")

(when (modulep! :editor evil +everywhere)
  (package! evil-textobj-tree-sitter
    :pin "ef4e57f2a9c29a4345f5ade015524d0736c61292"))
