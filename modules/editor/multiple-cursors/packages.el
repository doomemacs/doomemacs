;; -*- no-byte-compile: t; -*-
;;; editor/multiple-cursors/packages.el

(cond
 ((modulep! :editor evil)
  (package! evil-multiedit :pin "23b53bc8743fb82a8854ba907b1d277374c93a79")
  (package! evil-mc :pin "7e363dd6b0a39751e13eb76f2e9b7b13c7054a43"))

 ((package! multiple-cursors :pin "dd10cf2334333838e4550f091a75695448e26765")))
