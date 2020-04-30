;; -*- no-byte-compile: t; -*-
;;; editor/fold/packages.el

(package! hideshow :built-in t)

(package! vimish-fold :pin "63685239655a151181b9152e45478dad587f86f2")
(when (featurep! :editor evil)
  (package! evil-vimish-fold :pin "b6e0e6b91b8cd047e80debef1a536d9d49eef31a"))
