;; -*- no-byte-compile: t; -*-
;;; editor/fold/packages.el

(when (featurep! :editor evil)
  (package! evil-vimish-fold :pin "b6e0e6b91b"))
