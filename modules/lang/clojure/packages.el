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
(package! parseedn :pin "a09686fbb9113b8b1b4f20c9e1dc0d6fea01a64f")

;;; Core packages
(package! clojure-mode :pin "3453cd229b412227aaffd1dc2870fa8fa213c5b1")
(package! clj-refactor :pin "b5abe655e572a6ecfed02bb8164b64716ef76b8e")
(package! cider :pin "1ed5163433c991c00ea83fdd4447e8daf4aeccbe")
(when (modulep! :checkers syntax)
  (package! flycheck-clj-kondo :pin "ff7bed2315755cfe02ef471edf522e27b78cd5ca"))
(package! jet :pin "f007660c568e924e32d486a02aa4cd18203313cc")
(package! neil
  :recipe (:host github :repo "babashka/neil" :files ("*.el"))
  :pin "1dbac785cee4af8ad499839adbb83a8a297e7c70")
