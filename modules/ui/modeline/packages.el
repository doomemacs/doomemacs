;; -*- no-byte-compile: t; -*-
;;; ui/modeline/packages.el

(unless (featurep! +light)
  (package! doom-modeline :pin "ffedb34800db41f5e06e8a6f03698b56593c5024"))
(package! anzu :pin "bdb3da5028935a4aea55c40769bc191a81afb54e")
(when (featurep! :editor evil)
  (package! evil-anzu :pin "d3f6ed4773b48767bd5f4708c7f083336a8a8a86"))
