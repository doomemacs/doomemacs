;; -*- no-byte-compile: t; -*-
;;; editor/multiple-cursors/packages.el

(cond ((featurep! :editor evil)
       (package! evil-multiedit)
       (package! evil-mc))

      ((package! multiple-cursors)))

