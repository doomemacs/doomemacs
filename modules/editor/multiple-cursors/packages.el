;; -*- no-byte-compile: t; -*-
;;; editor/multiple-cursors/packages.el

(cond
 ((featurep! :editor evil)
  (package! evil-multiedit :pin "c3d43b1a65")
  (package! evil-mc :pin "007d471e26"))

 ((package! multiple-cursors :pin "b880554d04")))
