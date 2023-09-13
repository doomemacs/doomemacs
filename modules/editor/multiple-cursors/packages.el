;; -*- no-byte-compile: t; -*-
;;; editor/multiple-cursors/packages.el

(cond
 ((modulep! :editor evil)
  (package! evil-multiedit :pin "23b53bc8743fb82a8854ba907b1d277374c93a79")
  (package! evil-mc :pin "bdf893ea6f52fd0f10bece8ddae813658e17bbb4"))

 ((package! multiple-cursors :pin "234806c832994cadedb42596fe235e91bbd59e8c")))
