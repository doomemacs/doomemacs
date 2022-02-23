;; -*- no-byte-compile: t; -*-
;;; checkers/spell/packages.el

(if (not (featurep! +flyspell))
    (package! spell-fu :pin "8185467b24f05bceb428a0e9909651ec083cc54e")
  (package! flyspell-correct :pin "e9fde6f93af991b0528d6ed47d44bed470dc70af")
  (cond ((featurep! :completion ivy)
         (package! flyspell-correct-ivy))
        ((featurep! :completion helm)
         (package! flyspell-correct-helm))
        ((not (featurep! :completion vertico))
         (package! flyspell-correct-popup)))
  (package! flyspell-lazy :pin "0fc5996bcee20b46cbd227ae948d343c3bef7339"))
