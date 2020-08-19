;; -*- no-byte-compile: t; -*-
;;; editor/lispyville/packages.el

(package! lispy :pin "0a9dcfdfbc20cadbb9cb29b224dc64b8efdd7b70")
(when (featurep! :editor evil)
  (package! lispyville :pin "0f13f26cd6aa71f9fd852186ad4a00c4294661cd"))
