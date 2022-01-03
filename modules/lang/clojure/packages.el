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
(package! clojure-mode :pin "e31186843d06ea86f3771244d1cde0112f9e2079")
(package! clj-refactor :pin "12af23ad8b76519cb8b95eec4e8a5706d3186cd0")
(package! cider :pin "af2e1649981729930efbbf58af232b3e413da0af")
(when (featurep! :checkers syntax)
  (package! flycheck-clj-kondo :pin "d8a6ee9a16aa24b5be01f1edf9843d41bdc75555"))
