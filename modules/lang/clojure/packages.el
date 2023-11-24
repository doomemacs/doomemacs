;; -*- no-byte-compile: t; -*-
;;; lang/clojure/packages.el

;; HACK Fix #5577. Paredit is a cider dependency. We install paredit ourselves
;;      to get it from emacsmirror, because the original upstream is a custom
;;      git server with shallow clones disabled.
(package! paredit
  :recipe (:host github :repo "emacsmirror/paredit")
  :pin "9a2c4b37fc8c1c7bdbb1f86fdec874c0d0652e64")

;; HACK Forward declare these clj-refactor/cider deps so that their deps are
;;      byte-compiled first.
(package! parseclj :pin "74ff7d63fed92a3c859e474ae85f011e794b751a")
(package! parseedn :pin "c8f07926a688bfe995fde4460103915d401a1aff")

;;; Core packages
(package! clojure-mode :pin "25d713a67d8e0209ee74bfc0153fdf677697b43f")
(package! clj-refactor :pin "0a2a6cbc2e29177f4f55730637a357433a03fa38")
(package! cider :pin "120fd885d37c07137f1c162e8d522ab3eed1ac3f")
(when (and (modulep! :checkers syntax)
           (not (modulep! :checkers syntax +flymake)))
  (package! flycheck-clj-kondo :pin "ff7bed2315755cfe02ef471edf522e27b78cd5ca"))
(package! jet :pin "7d5157aac692fc761d8ed7a9f820fa6522136254")
(package! neil
  :recipe (:host github :repo "babashka/neil" :files ("*.el"))
  :pin "40993873bb4ef6d88af450e8a96d03275e266f6b")
