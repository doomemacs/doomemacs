;; -*- no-byte-compile: t; -*-
;;; editor/multiple-cursors/packages.el

(cond
 ((featurep! :editor evil)
  (package! evil-multiedit :pin "9f271e0e6048297692f80ed6c5ae8994ac523abc")
  (package! evil-mc :pin "4d4c0172e4c7f80acc1d0e73d5fb3e536929b262"))

 ((package! multiple-cursors :pin "b880554d04b8f61165afba7d4de19ac9e39bb7ab")))
