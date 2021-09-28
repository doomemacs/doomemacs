;; -*- no-byte-compile: t; -*-
;;; lang/java/packages.el

(package! android-mode :pin "d5332e339a1f5e30559a53feffb8442ca79265d6")
(package! groovy-mode :pin "84f89b68ec8f79bce0b3f5b29af155a85124e3a6")

(when (featurep! +meghanada)
  (package! meghanada :pin "6c57e8a0ae27e2929bb12572cf33059cd4ecbc04"))

(when (featurep! +eclim)
  (package! eclim :pin "222ddd48fcf0ee01592dec77c58e0cf3f2ea1100")
  (when (featurep! :completion company)
    (package! company-emacs-eclim :pin "222ddd48fcf0ee01592dec77c58e0cf3f2ea1100")))

(when (featurep! +lsp)
  (unless (featurep! :tools lsp +eglot)
    (package! lsp-java :pin "2a7d27e899edf7ad221a546ed67711ef5487f3ec")))
