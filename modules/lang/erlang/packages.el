;; -*- no-byte-compile: t; -*-
;;; lang/erlang/packages.el

(package! erlang :pin "e281016db92701ade29290e4f244c8f68fa53f6b")

(when (and (modulep! +tree-sitter) (treesit-available-p))
  (package! erlang-ts :pin "959907d26d32f7d23bdcbb6f9d06ccb2a5db54c3"))
