;; -*- no-byte-compile: t; -*-
;;; checkers/spell/packages.el

(if (not (modulep! +flyspell))
    (package! spell-fu :pin "aed6e87aa31013534b7a6cbedb26e4f29ccea735")
  (package! flyspell-correct :pin "7d7b6b01188bd28e20a13736ac9f36c3367bd16e")
  (cond ((modulep! :completion ivy)
         (package! flyspell-correct-ivy))
        ((modulep! :completion helm)
         (package! flyspell-correct-helm))
        ((not (modulep! :completion vertico))
         (package! flyspell-correct-popup)))
  (package! flyspell-lazy :pin "0fc5996bcee20b46cbd227ae948d343c3bef7339"))
