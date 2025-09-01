;; -*- no-byte-compile: t; -*-
;;; lang/kotlin/packages.el

(package! kotlin-mode :pin "fddd747e5b4736e8b27a147960f369b86179ddff")

(when (and (modulep! +tree-sitter) (treesit-available-p))
  (package! kotlin-ts-mode :pin "051c9ef534956c235343fb41546623ff87a1695b"))

(when (modulep! :checkers syntax -flymake)
  (package! flycheck-kotlin :pin "a2a6abb9a7f85c6fb15ce327459ec3c8ff780188"))
