;; -*- no-byte-compile: t; -*-
;;; lang/janet/packages.el

(package! janet-mode
  :recipe (:files ("*.el"))
  :pin "9e3254a0249d720d5fa5603f1f8c3ed0612695af")

(when (and (modulep! +tree-sitter) (treesit-available-p))
  (package! janet-ts-mode
    :recipe (:host github :repo "sogaiu/janet-ts-mode")
    :pin "0e4d04d6487486104c3d65576e7a1480ce52b3fd"))
