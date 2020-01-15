;; -*- no-byte-compile: t; -*-
;;; editor/fold/packages.el

(when (featurep! :editor evil)
  (package! evil-vimish-fold :pin "c617fecb91303f8c63f85a6101a503fdc88aae84"))
