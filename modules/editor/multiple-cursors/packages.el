;; -*- no-byte-compile: t; -*-
;;; editor/multiple-cursors/packages.el

(cond
 ((featurep! :editor evil)
  (package! evil-multiedit :pin "15a47540db21f2b34e8ed4fbeba33c5a63d8f317")
  (package! evil-mc :pin "246aecc17481dd23c172a9b845f02a9d9e322c7f"))

 ((package! multiple-cursors :pin "8a60fc7ef0ae6e5ca089a7c95264cd0ae83e7274")))
