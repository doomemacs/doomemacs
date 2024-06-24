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
(package! parseclj :pin "6af22372e0fe14df882dd300b22b12ba2d7e00b0")
(package! parseedn :pin "3407e4530a367b6c2b857dae261cdbb67a440aaa")

;;; Core packages
(package! clojure-mode :pin "59888c84b61081e9b0085e388f55132925a66e7a")
(package! clj-refactor :pin "dc1bbc8cdaa723bdbb6669ea7d280625c370755d")
(package! cider :pin "105da319b09a436552f1b3c6194cbbc833017dd2")
(when (and (modulep! :checkers syntax)
           (not (modulep! :checkers syntax +flymake)))
  (package! flycheck-clj-kondo :pin "e38c67ba9db1ea1cbe1b61ab39b506c05efdcdbf"))
(package! jet :pin "7d5157aac692fc761d8ed7a9f820fa6522136254")
(package! neil
  :recipe (:host github :repo "babashka/neil" :files ("*.el"))
  :pin "a38be9f0821b828f7c75a527bc4cfb256f6aa8af")
