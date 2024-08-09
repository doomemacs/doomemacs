;; -*- no-byte-compile: t; -*-
;;; editor/multiple-cursors/packages.el

(cond
 ((modulep! :editor evil)
  (package! evil-multiedit :pin "23b53bc8743fb82a8854ba907b1d277374c93a79")
  (package! evil-mc :pin "cff3374bfe1b7b1932743425d7fc5d4ab66d747e"))

 ((package! multiple-cursors :pin "c870c18462461df19382ecd2f9374c8b902cd804")))
