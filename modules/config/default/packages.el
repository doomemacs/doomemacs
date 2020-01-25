;; -*- no-byte-compile: t; -*-
;;; config/default/packages.el

(package! avy :pin "cf95ba9582")
(package! drag-stuff :pin "6d06d846cd")
(package! link-hint :pin "8d8f9505f8")

(unless (featurep! :editor evil)
  (package! expand-region :pin "0fa7c2d349"))
