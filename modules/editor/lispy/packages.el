;; -*- no-byte-compile: t; -*-
;;; editor/lispyville/packages.el

(package! lispy :pin "cdaa9c70ca39a880163cbbce924bb46cc56b9fa4")
(when (featurep! :editor evil)
  (package! lispyville :pin "25a70126ea807653e0a8c512d4128c90ed673d7a"))
