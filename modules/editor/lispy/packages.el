;; -*- no-byte-compile: t; -*-
;;; editor/lispyville/packages.el

(package! lispy
  :recipe (:host github :repo "enzuru/lispy")
  :pin "c42d3737c57de86a5aa321d7efa29390aeedcf6f")
(when (modulep! :editor evil)
  (package! lispyville :pin "14ee8711d58b649aeac03581d22b10ab077f06bd"))
