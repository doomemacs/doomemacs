;; -*- no-byte-compile: t; -*-
;;; tools/tree-sitter/packages.el

(package! tree-sitter :pin "e0ce25406706e0a164bd565a391f5bb8f54f995f")
(package! tree-sitter-langs :pin "b7895ca759563f3c7c3b928eb4f816bb4099d866")
(package! tree-sitter-indent :pin "4ef246db3e4ff99f672fe5e4b416c890f885c09e")

(when (modulep! :editor evil +everywhere)
  (package! evil-textobj-tree-sitter
    :pin "a19ab9d89a00f4a04420f9b5d61b66f04fea5261"))
