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
(package! parseclj :pin "4d0e780e00f1828b00c43099e6eebc6582998f72")
(package! parseedn :pin "a09686fbb9113b8b1b4f20c9e1dc0d6fea01a64f")

;;; Core packages
(package! clojure-mode :pin "b6f41d74904daa9312648f3a7bea7a72fd8e140b")
(package! clj-refactor :pin "f368c56c83843396b160440f472a661a3b639862")
(package! cider :pin "b9e1cc26e2afda003a4b6c1f2a26e04f1c45f3d0")
(when (modulep! :checkers syntax)
  (package! flycheck-clj-kondo :pin "ff7bed2315755cfe02ef471edf522e27b78cd5ca"))
