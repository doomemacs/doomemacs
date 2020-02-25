;; -*- no-byte-compile: t; -*-
;;; editor/fold/packages.el

(package! hideshow :built-in t)

(package! vimish-fold :pin "d3248a41a7")
(when (featurep! :editor evil)
  (package! evil-vimish-fold :pin "b6e0e6b91b"))
