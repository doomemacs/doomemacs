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
(package! parseclj :pin "4d0e780e00f1828b00c43099e6eebc6582998f72")
(package! parseedn :pin "c8f07926a688bfe995fde4460103915d401a1aff")

;;; Core packages
(package! clojure-mode :pin "525fc1b131b1fc537aa82d83d9eb2ea833cface6")
(package! clj-refactor :pin "b476345c580ae7cbc6b356ba0157db782684c47f")
(package! cider :pin "944d6773ac254d9fcac55c05489bff3d91d91402")
(when (and (modulep! :checkers syntax)
           (not (modulep! :checkers syntax +flymake)))
  (package! flycheck-clj-kondo :pin "d8a6ee9a16aa24b5be01f1edf9843d41bdc75555"))
(package! jet :pin "f007660c568e924e32d486a02aa4cd18203313cc")
(package! neil
  :recipe (:host github :repo "babashka/neil" :files ("*.el"))
  :pin "f597921dcbf4774d799be62d8fbbce7171b12c3f")
