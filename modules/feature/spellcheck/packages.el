;; -*- no-byte-compile: t; -*-
;;; feature/spellcheck/packages.el

(package! flyspell-correct)
(cond ((featurep! :completion ivy)
       (package! flyspell-correct-ivy))
      ((featurep! :completion helm)
       (package! flyspell-correct-helm))
      (t
       (package! flyspell-correct-popup)))

