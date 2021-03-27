;; -*- no-byte-compile: t; -*-
;;; checkers/spell/packages.el

(if (not (featurep! +flyspell))
    (package! spell-fu :pin "30c5eea9440915c2795670ecdbf836f3cb9dcc1b")
  (package! flyspell-correct :pin "d19a090b978a249fc8f6d8b14309a5705a6bb483")
  (cond ((featurep! :completion ivy)
         (package! flyspell-correct-ivy))
        ((featurep! :completion helm)
         (package! flyspell-correct-helm))
        ((package! flyspell-correct-popup)))
  (package! flyspell-lazy :pin "0fc5996bcee20b46cbd227ae948d343c3bef7339"))
