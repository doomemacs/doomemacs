;; -*- no-byte-compile: t; -*-
;;; editor/lispyville/packages.el

(package! lispy :pin "38a7df4cbb16cfe3d62dc8ea98b50e2d9a572e58")
(when (featurep! :editor evil)
  (package! lispyville :pin "89316f01822b2135e52ca27fd308d207ef618052"))
