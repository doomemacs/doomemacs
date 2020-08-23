;; -*- no-byte-compile: t; -*-
;;; checkers/spell/packages.el

(if (not (featurep! +flyspell))
    (package! spell-fu :pin "e94d01cdc822e02968971cde09276047a5d55772")
  (package! flyspell-correct :pin "dea1290a371c540dde7b8d0eef7a12d92f7a0b83")
  (cond ((featurep! :completion ivy)
         (package! flyspell-correct-ivy))
        ((featurep! :completion helm)
         (package! flyspell-correct-helm))
        ((package! flyspell-correct-popup)))
  (package! flyspell-lazy :pin "3ebf68cc9eb10c972a2de8d7861cbabbbce69570"))
