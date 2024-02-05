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
(package! clojure-mode :pin "222fdafa2add56a171ded245339a383e5e3078ec")
(package! clj-refactor :pin "fa3efe18e7150df5153a7d05c54e96d59398a0a8")
(package! cider :pin "aa26d62ac59930079e47e652ccd73e8e447defd5")
(when (and (modulep! :checkers syntax)
           (not (modulep! :checkers syntax +flymake)))
  (package! flycheck-clj-kondo :pin "9089ade9e01b091139321c78ad75946944ff845d"))
(package! jet :pin "7d5157aac692fc761d8ed7a9f820fa6522136254")
(package! neil
  :recipe (:host github :repo "babashka/neil" :files ("*.el"))
  :pin "a1db63d420b85db814207113ca4a0b4b959073cc")
