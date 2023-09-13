;; -*- no-byte-compile: t; -*-
;;; editor/multiple-cursors/packages.el

(cond
 ((modulep! :editor evil)
  (package! evil-multiedit :pin "23b53bc8743fb82a8854ba907b1d277374c93a79")
  (package! evil-mc :pin "bdf893ea6f52fd0f10bece8ddae813658e17bbb4a4cc31c464d246f92931c4cb720f"))

 ((package! multiple-cursors :pin "16223efc2d6dece2d43bbccc189d7a4bab6de571")))
