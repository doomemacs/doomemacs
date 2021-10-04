;; -*- no-byte-compile: t; -*-
;;; lang/clojure/packages.el

;; HACK Fix #5577. Paredit is a cider dependency. We install paredit ourselves
;;      to get it from emacsmirror, because the original upstream is a custom
;;      git server with shallow clones disabled.
(package! paredit
  :recipe (:host github :repo "emacsmirror/paredit")
  :pin "8330a41e8188fe18d3fa805bb9aa529f015318e8")

;; HACK Forward declare these clj-refactor/cider deps so that their deps are
;;      byte-compiled first.
(package! parseclj :pin "fcebf650759929256ec9c4bb83b677240622be8a")
(package! parseedn :pin "b00eb42a1c10f19ba0f6ff5f8cb9e3ac05285dbf")

;;; Core packages
(package! clojure-mode :pin "e1dc7caee76d117a366f8b8b1c2da7e6400636a8")
(package! clj-refactor :pin "23743432c39be9b62630f3f6468ac36ebc12aaff")
(package! cider :pin "2b8bde358063e782771f2f12bdf32374d68a7174")
(when (featurep! :checkers syntax)
  (package! flycheck-clj-kondo :pin "a558bda44c4cb65b69fa53df233e8941ebd195c5"))
