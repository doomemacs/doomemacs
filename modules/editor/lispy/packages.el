;; -*- no-byte-compile: t; -*-
;;; editor/lispyville/packages.el

(package! lispy :pin "df1b7e614fb0f73646755343e8892ddda310f427")
(when (featurep! :editor evil)
  (package! lispyville :pin "9c14bed0359f659e246d345c706f895737c3d172"))
