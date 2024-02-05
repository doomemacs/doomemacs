;; -*- no-byte-compile: t; -*-
;;; lang/java/packages.el

(package! android-mode :pin "d5332e339a1f5e30559a53feffb8442ca79265d6")
(package! groovy-mode :pin "7b8520b2e2d3ab1d62b35c426e17ac25ed0120bb")

(when (modulep! +meghanada)
  (package! meghanada :pin "fb29746e442e3d7b903759d15977d142a4bf2131"))

(when (modulep! +eclim)
  (package! eclim :pin "222ddd48fcf0ee01592dec77c58e0cf3f2ea1100")
  (when (modulep! :completion company)
    (package! company-emacs-eclim :pin "222ddd48fcf0ee01592dec77c58e0cf3f2ea1100")))

(when (modulep! +lsp)
  (unless (modulep! :tools lsp +eglot)
    (package! lsp-java :pin "c962a3b3ac2beabdf1ce83b815396d6c38e3cefa")))
