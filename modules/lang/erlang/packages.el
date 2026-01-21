;; -*- no-byte-compile: t; -*-
;;; lang/erlang/packages.el

(package! erlang :pin "2b2b39797f628712a52f7180452e8b12e03a42ef")

(when (and (modulep! +tree-sitter) (treesit-available-p))
  (package! erlang-ts :pin "959907d26d32f7d23bdcbb6f9d06ccb2a5db54c3"))
