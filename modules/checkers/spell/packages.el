;; -*- no-byte-compile: t; -*-
;;; checkers/spell/packages.el

(if (not (featurep! +flyspell))
    (package! spell-fu :pin "a7db58747131dca2eee0e0757c3d254d391ddd1c")
  (package! flyspell-correct :pin "d19a090b978a249fc8f6d8b14309a5705a6bb483")
  (cond ((featurep! :completion ivy)
         (package! flyspell-correct-ivy))
        ((featurep! :completion helm)
         (package! flyspell-correct-helm))
        ((package! flyspell-correct-popup)))
  (package! flyspell-lazy :pin "d57382cf06462931cb354f5630469425fce56396"))
