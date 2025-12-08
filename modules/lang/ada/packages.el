;; -*- no-byte-compile: t; -*-
;;; lang/ada/packages.el

(package! ada-mode :pin "ce8a2dfebc2b738f32b61dbe2668f7acb885db93")
(when (and (modulep! +tree-sitter) (treesit-available-p))
  (package! ada-ts-mode :pin "52e0fd11604ab1d51a34c89e05692446d9dc5ecb"))
(package! gpr-mode :pin "03141c6b9a39c31a6e759b594b41617b97b02753")
(when (and (modulep! +tree-sitter) (treesit-available-p))
  (package! gpr-ts-mode :pin "b8aeca2c8fd5ed370dad0676da8f380627c916d5"))
