;; -*- no-byte-compile: t; -*-
;;; tools/tree-sitter/packages.el

(package! tree-sitter :recipe (:branch "master") :pin "1c455b0953da06c40fcf1f21f1ac0c6e179b46d0")
(package! tree-sitter-langs :pin "becd29c756a3272bc91d09de642df99a0fca6cee")
(package! tree-sitter-indent :pin "4ef246db3e4ff99f672fe5e4b416c890f885c09e")

(when (modulep! :editor evil +everywhere)
  (package! evil-textobj-tree-sitter
    :pin "bce236e5d2cc2fa4eae7d284ffd19ad18d46349a"))
