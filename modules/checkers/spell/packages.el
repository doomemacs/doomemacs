;; -*- no-byte-compile: t; -*-
;;; checkers/spell/packages.el

(if (modulep! -flyspell)
    (package! spell-fu
      :recipe (:host github :repo "emacsmirror/spell-fu")
      :pin "d465d70126d7ff8e37013ef942c292aaa1ca23f3")
  (package! flyspell-correct :pin "1e7a5a56362dd875dddf848b9a9e25d1395b9d37")
  (cond ((modulep! :completion ivy)
         (package! flyspell-correct-ivy))
        ((modulep! :completion helm)
         (package! flyspell-correct-helm))
        ((not (modulep! :completion vertico))
         (package! flyspell-correct-popup)))
  (package! flyspell-lazy :pin "0fc5996bcee20b46cbd227ae948d343c3bef7339"))
