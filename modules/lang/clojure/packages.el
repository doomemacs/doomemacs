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
(package! parseclj :pin "a8c4cf30fb68b66ae51541462a8b21753229a6e5")
(package! parseedn :pin "e5ba280d1fb7b408d54062d4eac545326e850172")

;;; Core packages
(package! clojure-mode :pin "e1dc7caee76d117a366f8b8b1c2da7e6400636a8")
(package! clj-refactor :pin "4cb75bd6a2fcb376455e8b4f3edee509f87b86b8")
(package! cider :pin "0a9d0ef429e76ee36c34e116c4633c69cea96c67")
(when (featurep! :checkers syntax)
  (package! flycheck-clj-kondo :pin "a558bda44c4cb65b69fa53df233e8941ebd195c5"))
