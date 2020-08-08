;; -*- no-byte-compile: t; -*-
;;; ui/modeline/packages.el

(unless (featurep! +light)
  (package! doom-modeline :pin "881e099fe02e3dd5ad66c51156fcdcc60e093a9b"))
(package! anzu :pin "7b8688c84d6032300d0c415182c7c1ad6cb7f819")
(when (featurep! :editor evil)
  (package! evil-anzu :pin "d3f6ed4773b48767bd5f4708c7f083336a8a8a86"))
