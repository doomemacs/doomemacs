;; -*- no-byte-compile: t; -*-
;;; editor/multiple-cursors/packages.el

(cond
 ((featurep! :editor evil)
  (package! evil-multiedit :pin "9f271e0e60")
  (package! evil-mc :pin "4d4c0172e4"))

 ((package! multiple-cursors :pin "b880554d04")))
