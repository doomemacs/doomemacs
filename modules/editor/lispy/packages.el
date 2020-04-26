;; -*- no-byte-compile: t; -*-
;;; editor/lispyville/packages.el

(package! lispy :pin "c7e282ae06")
(when (featurep! :editor evil)
  (package! lispyville :pin "25a70126ea"))
