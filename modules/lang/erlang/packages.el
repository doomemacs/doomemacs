;; -*- no-byte-compile: t; -*-
;;; lang/erlang/packages.el

(package! erlang :pin "f80e9c1c4a3f271e39fcdb0be4ddbd88da3118c7")

(when (and (modulep! +tree-sitter) (treesit-available-p))
  (package! erlang-ts :pin "959907d26d32f7d23bdcbb6f9d06ccb2a5db54c3"))
