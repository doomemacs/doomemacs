;; -*- no-byte-compile: t; -*-
;;; config/default/packages.el

(package! avy :pin "3bf83140fa")
(package! drag-stuff :pin "6d06d846cd")
(package! link-hint :pin "0d9cabcdb7")

(unless (featurep! :editor evil)
  (package! expand-region :pin "ea6b4cbb99"))
