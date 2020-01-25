;; -*- no-byte-compile: t; -*-
;;; checkers/spell/packages.el

(package! flyspell-correct :pin "b0353a41a7")
(cond ((featurep! :completion ivy)
       (package! flyspell-correct-ivy :pin "b0353a41a7"))
      ((featurep! :completion helm)
       (package! flyspell-correct-helm :pin "b0353a41a7"))
      ((package! flyspell-correct-popup :pin "b0353a41a7")))
