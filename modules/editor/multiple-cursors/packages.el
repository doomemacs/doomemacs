;; -*- no-byte-compile: t; -*-
;;; editor/multiple-cursors/packages.el

(cond
 ((featurep! :editor evil)
  (package! evil-multiedit :pin "c3d43b1a65c193755dae2c41ce5c072c4c01b35d")
  (package! evil-mc :pin "007d471e26b44e692250804f82f06ebbd27b6ec4"))

 ((package! multiple-cursors :pin "b880554d04b8f61165afba7d4de19ac9e39bb7ab")))
