;; -*- no-byte-compile: t; -*-
;;; editor/lispyville/packages.el

(package! lispy :pin "bf315768020f98f6139d5f722bd365f1ddd1fb52")
(when (featurep! :editor evil)
  (package! lispyville :pin "9c14bed0359f659e246d345c706f895737c3d172"))
