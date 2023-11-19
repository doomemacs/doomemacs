;; -*- no-byte-compile: t; -*-
;;; lang/java/packages.el

(package! android-mode :pin "d5332e339a1f5e30559a53feffb8442ca79265d6")
(package! groovy-mode :pin "c612ac1e9f742856914ad6e8eb9e9dc169f489ab")

(when (modulep! +meghanada)
  (package! meghanada :pin "59c46cabb7eee715fe810ce59424934a1286df84"))

(when (modulep! +eclim)
  (package! eclim :pin "222ddd48fcf0ee01592dec77c58e0cf3f2ea1100")
  (when (modulep! :completion company)
    (package! company-emacs-eclim :pin "222ddd48fcf0ee01592dec77c58e0cf3f2ea1100")))

(when (modulep! +lsp)
  (unless (modulep! :tools lsp +eglot)
    (package! lsp-java :pin "449673da7221a30f1b1756cedcc48b9a2b52a51e")))
