;; -*- no-byte-compile: t; -*-
;;; lang/ada/packages.el

(package! ada-mode :pin "ce8a2dfebc2b738f32b61dbe2668f7acb885db93")
(when (and (modulep! +tree-sitter) (treesit-available-p))
  (package! ada-ts-mode :pin "d0c1c124b236b402b884188948cb1f3502ef8779"))
