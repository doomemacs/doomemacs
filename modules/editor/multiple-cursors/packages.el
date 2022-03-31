;; -*- no-byte-compile: t; -*-
;;; editor/multiple-cursors/packages.el

(cond
 ((featurep! :editor evil)
  (package! evil-multiedit :pin "23b53bc8743fb82a8854ba907b1d277374c93a79")
  (package! evil-mc :pin "63fd2fe0c213a4cc31c464d246f92931c4cb720f"))

 ((package! multiple-cursors :pin "aae47aebc0ae829211fa1e923232715d8e327b36")))
