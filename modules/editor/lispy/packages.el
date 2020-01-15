;; -*- no-byte-compile: t; -*-
;;; editor/lispyville/packages.el

(package! lispy :pin "8db042d40bccc628dd406c1fe712431fb76b3288")

(when (featurep! :editor evil)
  (package! lispyville :pin "56198f1c4488a52a0d0512c717dff36e8b9fbfd0"))
