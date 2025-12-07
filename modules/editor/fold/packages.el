;; -*- no-byte-compile: t; -*-
;;; editor/fold/packages.el

(package! hideshow :built-in t)

(package! vimish-fold :pin "f71f374d28a83e5f15612fa64aac1b2e78be2dcd")
(when (modulep! :editor evil)
  (package! evil-vimish-fold :pin "b6e0e6b91b8cd047e80debef1a536d9d49eef31a"))
(when (modulep! :tools tree-sitter)
  (package! treesit-fold :pin "8182ae243a57c3f418ea64ac1e594a210f5dcf16"))
