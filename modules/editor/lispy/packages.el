;; -*- no-byte-compile: t; -*-
;;; editor/lispyville/packages.el

(package! lispy :pin "097dd66e662c3eee90d112d88bac5345d26e508f")
(when (modulep! :editor evil)
  (package! lispyville :pin "14ee8711d58b649aeac03581d22b10ab077f06bd"))
