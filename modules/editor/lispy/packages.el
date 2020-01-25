;; -*- no-byte-compile: t; -*-
;;; editor/lispyville/packages.el

(package! lispy :pin "d6b19fe2c3")
(when (featurep! :editor evil)
  (package! lispyville :pin "56198f1c44"))
