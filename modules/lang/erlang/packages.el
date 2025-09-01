;; -*- no-byte-compile: t; -*-
;;; lang/erlang/packages.el

(package! erlang :pin "2a970bf7257c6571536cc998fe01ff69b874e1c1")

(when (and (modulep! +tree-sitter) (treesit-available-p))
  (package! erlang-ts :pin "eb579dd55fbb2cf721290939e7b3a50be19c0305"))
