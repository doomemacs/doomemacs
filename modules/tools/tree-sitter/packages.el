;; -*- no-byte-compile: t; -*-
;;; tools/tree-sitter/packages.el

(package! tree-sitter :recipe (:branch "master") :pin "f3b6e78cd426c4f5de6b9d6a85b94cbe31c83495")
(package! tree-sitter-langs :pin "2ff446b4b813543b7a90015808d38f362f039b10")
(package! tree-sitter-indent :pin "4ef246db3e4ff99f672fe5e4b416c890f885c09e")

(when (modulep! :editor evil +everywhere)
  (package! evil-textobj-tree-sitter
    :pin "bce236e5d2cc2fa4eae7d284ffd19ad18d46349a"))
