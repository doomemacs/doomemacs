;; -*- no-byte-compile: t; -*-
;;; editor/lispyville/packages.el

(package! lispy :pin "5c8a59ae7dd3dd342e7c86a8c0acdbd13e2989f3")
(when (featurep! :editor evil)
  (package! lispyville :pin "0f13f26cd6aa71f9fd852186ad4a00c4294661cd"))
