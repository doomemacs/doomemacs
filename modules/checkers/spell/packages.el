;; -*- no-byte-compile: t; -*-
;;; checkers/spell/packages.el

(package! flyspell-correct :pin "e765d1a3d9")
(cond ((featurep! :completion ivy)
       (package! flyspell-correct-ivy :pin "e765d1a3d9"))
      ((featurep! :completion helm)
       (package! flyspell-correct-helm :pin "e765d1a3d9"))
      ((package! flyspell-correct-popup :pin "e765d1a3d9")))

(package! flyspell-lazy :pin "3ebf68cc9e")
