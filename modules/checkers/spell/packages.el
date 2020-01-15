;; -*- no-byte-compile: t; -*-
;;; checkers/spell/packages.el

(package! flyspell-correct :pin "7b4cf8c9ba5ac65e3bb2b62f5b72d45f4c9cf7b6")
(cond ((featurep! :completion ivy)
       (package! flyspell-correct-ivy))
      ((featurep! :completion helm)
       (package! flyspell-correct-helm))
      ((package! flyspell-correct-popup)))
