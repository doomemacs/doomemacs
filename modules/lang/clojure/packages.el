;; -*- no-byte-compile: t; -*-
;;; lang/clojure/packages.el

;; HACK Fix #5577. Paredit is a cider dependency. We install paredit ourselves
;;      to get it from emacsmirror, because the original upstream is a custom
;;      git server with shallow clones disabled.
(package! paredit
  :recipe (:host github :repo "emacsmirror/paredit")
  :pin "e4a67f4f23ba936b4bdc8d7e66bd8c6729064558")

;; HACK Forward declare these clj-refactor/cider deps so that their deps are
;;      byte-compiled first.
(package! parseclj :pin "4d0e780e00f1828b00c43099e6eebc6582998f72")
(package! parseedn :pin "a09686fbb9113b8b1b4f20c9e1dc0d6fea01a64f")

;;; Core packages
(package! clojure-mode :pin "414157c3e523e80cc44dca8f86f1853122ee5f6b")
(package! clj-refactor :pin "db89b558710d31d4b9dfe7c4d2a07df9b498d4a6")
(package! cider :pin "92c1b6de20805a9caa556cc671ac39080e4e89cc")
(when (modulep! :checkers syntax)
  (package! flycheck-clj-kondo :pin "ff7bed2315755cfe02ef471edf522e27b78cd5ca"))
