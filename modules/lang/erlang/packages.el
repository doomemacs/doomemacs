;; -*- no-byte-compile: t; -*-
;;; lang/erlang/packages.el

(package! erlang :pin "b9d3ec017da091168918a2901a1cef4cb062fd2a")

(when (and (modulep! +tree-sitter) (treesit-available-p))
  (package! erlang-ts :pin "eb579dd55fbb2cf721290939e7b3a50be19c0305"))
