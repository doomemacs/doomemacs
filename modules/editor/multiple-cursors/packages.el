;; -*- no-byte-compile: t; -*-
;;; editor/multiple-cursors/packages.el

(cond
 ((featurep! :editor evil)
  (package! evil-multiedit :pin "50179bfb269b35d248a77105f7feb7bbc87fd302")
  (package! evil-mc :pin "246aecc17481dd23c172a9b845f02a9d9e322c7f"))

 ((package! multiple-cursors :pin "588daf8c520f4545323e36b8900f02693ddcf5d3")))
