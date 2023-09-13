;; -*- no-byte-compile: t; -*-
;;; editor/lispyville/packages.el

(package! lispy :pin "fe44efd21573868638ca86fc8313241148fabbe3")
(when (modulep! :editor evil)
  (package! lispyville :pin "14ee8711d58b649aeac03581d22b10ab077f06bd"))
