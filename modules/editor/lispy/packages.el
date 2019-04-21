;; -*- no-byte-compile: t; -*-
;;; editor/lispyville/packages.el

(package! lispy)

(when (featurep! :editor evil)
  (package! lispyville))
