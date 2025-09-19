;; -*- no-byte-compile: t; -*-
;;; lang/ada/packages.el

(package! ada-mode :pin "ce8a2dfebc2b738f32b61dbe2668f7acb885db93")
(when (and (modulep! +tree-sitter) (treesit-available-p))
  (package! ada-ts-mode :pin "d0c1c124b236b402b884188948cb1f3502ef8779"))
(package! gpr-mode :pin "03141c6b9a39c31a6e759b594b41617b97b02753")
(when (and (modulep! +tree-sitter) (treesit-available-p))
  (package! gpr-ts-mode :pin "b8aeca2c8fd5ed370dad0676da8f380627c916d5"))
