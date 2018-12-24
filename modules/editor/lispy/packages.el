;; -*- no-byte-compile: t; -*-
;;; editor/lispyville/packages.el

(package! lispy)

(when (featurep! :feature evil)
  (package! lispyville))
