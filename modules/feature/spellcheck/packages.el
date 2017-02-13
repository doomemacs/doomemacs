;; -*- no-byte-compile: t; -*-
;;; feature/spellcheck/packages.el

(@package flyspell-correct)
(@package flyspell-correct-popup)

(cond ((@featurep :completion ivy)
       (@package flyspell-correct-ivy))
      ((@featurep :completion helm)
       (@package flyspell-correct-helm))
      ((@featurep :emacs ido)
       (@package flyspell-correct-ido)))


