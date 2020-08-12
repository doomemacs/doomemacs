;; -*- no-byte-compile: t; -*-
;;; lang/java/packages.el

(package! android-mode :pin "d5332e339a1f5e30559a53feffb8442ca79265d6")
(package! groovy-mode :pin "26da902d1158c0312628d57578109be54eca2415")

(when (featurep! +meghanada)
  (package! meghanada :pin "e119c7b3271281d60892b80a0cc6488503baf38f"))

(when (featurep! +eclim)
  (package! eclim :pin "222ddd48fcf0ee01592dec77c58e0cf3f2ea1100")
  (when (featurep! :completion company)
    (package! company-emacs-eclim :pin "222ddd48fcf0ee01592dec77c58e0cf3f2ea1100")))

(when (featurep! +lsp)
  (package! lsp-java :pin "260016236fa0520b5b6ec7f51ca2086288524cba"))
