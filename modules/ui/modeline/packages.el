;; -*- no-byte-compile: t; -*-
;;; ui/modeline/packages.el

(unless (featurep! +light)
  (package! doom-modeline :pin "ffbaaee832f1c97ff608bc4959b408997d959b7d"))
(package! anzu :pin "7b8688c84d6032300d0c415182c7c1ad6cb7f819")
(when (featurep! :editor evil)
  (package! evil-anzu :pin "d3f6ed4773b48767bd5f4708c7f083336a8a8a86"))
(when (featurep! +keycast)
  (package! keycast :pin "038475c178e90c7bad64d113db26d42cad60e149"))
