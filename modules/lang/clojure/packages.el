;; -*- no-byte-compile: t; -*-
;;; lang/clojure/packages.el

;; HACK Fix #5577. Paredit is a cider dependency. We install paredit ourselves
;;      to get it from emacsmirror, because the original upstream is a custom
;;      git server with shallow clones disabled.
(package! paredit
  :recipe (:host github :repo "emacsmirror/paredit")
  :pin "037b9b8acbca75151f133b6c0f7f3ff97d9042e5")

;; HACK Forward declare these clj-refactor/cider deps so that their deps are
;;      byte-compiled first.
(package! parseclj :pin "6af22372e0fe14df882dd300b22b12ba2d7e00b0")
(package! parseedn :pin "3407e4530a367b6c2b857dae261cdbb67a440aaa")

;;; Core packages
(package! clojure-mode :pin "59888c84b61081e9b0085e388f55132925a66e7a")
(package! clj-refactor :pin "dc1bbc8cdaa723bdbb6669ea7d280625c370755d")
(package! cider :pin "8fdb53e8be764704f5023fa024d023bffedf5a8b")
(when (modulep! :checkers syntax -flymake)
  (package! flycheck-clj-kondo :pin "e38c67ba9db1ea1cbe1b61ab39b506c05efdcdbf"))
(package! jet :pin "c9a92675efd802f37df5e3eab7858dbbeced6ea4")
(package! neil
  :recipe (:host github :repo "babashka/neil" :files ("*.el"))
  :pin "6728367eff5f03e4e3e13d1b155aef77a5297bea")
