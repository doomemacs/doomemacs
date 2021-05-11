;; -*- no-byte-compile: t; -*-
;;; lang/java/packages.el

(package! android-mode :pin "d5332e339a1f5e30559a53feffb8442ca79265d6")
(package! groovy-mode :pin "26da902d1158c0312628d57578109be54eca2415")

(when (featurep! +meghanada)
  (package! meghanada :pin "6c57e8a0ae27e2929bb12572cf33059cd4ecbc04"))

(when (featurep! +eclim)
  (package! eclim :pin "222ddd48fcf0ee01592dec77c58e0cf3f2ea1100")
  (when (featurep! :completion company)
    (package! company-emacs-eclim :pin "222ddd48fcf0ee01592dec77c58e0cf3f2ea1100")))

(when (featurep! +lsp)
  (unless (featurep! :tools lsp +eglot)
    (package! lsp-java :pin "9685334086c0b09d2bb16f631fb368f4ce931764")))
