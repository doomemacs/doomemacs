;; -*- no-byte-compile: t; -*-
;;; lang/erlang/packages.el

(package! erlang :pin "ea2bfd02bed1f003cd73c0eaa5d87dde153e9124")

(when (and (modulep! +tree-sitter) (treesit-available-p))
  (package! erlang-ts :pin "959907d26d32f7d23bdcbb6f9d06ccb2a5db54c3"))
