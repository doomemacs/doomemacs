;; -*- no-byte-compile: t; -*-
;;; config/default/packages.el

(package! avy)
(package! drag-stuff)
(package! link-hint)

(unless (featurep! :editor evil)
  (package! expand-region))
