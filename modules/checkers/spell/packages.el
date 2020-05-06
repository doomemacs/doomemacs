;; -*- no-byte-compile: t; -*-
;;; checkers/spell/packages.el

(package! flyspell-correct :pin "fd8ac7a4f922ce5ea1cc5d4583a7d584847cb6b5")
(cond ((featurep! :completion ivy)
       (package! flyspell-correct-ivy))
      ((featurep! :completion helm)
       (package! flyspell-correct-helm))
      ((package! flyspell-correct-popup)))

(package! flyspell-lazy :pin "3ebf68cc9eb10c972a2de8d7861cbabbbce69570")
