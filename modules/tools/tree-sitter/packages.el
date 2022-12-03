;; -*- no-byte-compile: t; -*-
;;; tools/tree-sitter/packages.el

(package! tree-sitter
  :pin "c3fe96a103a766256ba62120eb638eef8e9a9802")

(package! tree-sitter-langs
  :pin "deb2d8674be8f777ace50e15c7c041aeddb1d0b2")

(when (modulep! :editor evil +everywhere)
  (package! evil-textobj-tree-sitter
    :pin "9dce8dab68c954ae32095328cf898eb856fc341a"))
