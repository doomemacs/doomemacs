;; -*- no-byte-compile: t; -*-
;;; editor/lispyville/packages.el

(package! lispy :pin "e9731aa95581951ab2cbfaed28f0ac7d71124ac0")
(when (featurep! :editor evil)
  (package! lispyville :pin "9c14bed0359f659e246d345c706f895737c3d172"))
