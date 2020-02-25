;; -*- no-byte-compile: t; -*-
;;; config/default/packages.el

(package! avy :pin "cf95ba9582")
(package! drag-stuff :pin "6d06d846cd")
(package! link-hint :pin "0d9cabcdb7")

(unless (featurep! :editor evil)
  (package! expand-region :pin "1603d01fbf"))
