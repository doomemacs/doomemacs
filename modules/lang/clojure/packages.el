;; -*- no-byte-compile: t; -*-
;;; lang/clojure/packages.el

;; HACK Fix #5577. Paredit is a cider dependency. We install paredit ourselves
;;      to get it from emacsmirror, because the original upstream is a custom
;;      git server with shallow clones disabled.
(package! paredit
  :recipe (:host github :repo "emacsmirror/paredit")
  :pin "af075775af91f2dbc63b915d762b4aec092946c4")

;; HACK Forward declare these clj-refactor/cider deps so that their deps are
;;      byte-compiled first.
(package! parseclj :pin "6af22372e0fe14df882dd300b22b12ba2d7e00b0")
(package! parseedn :pin "3407e4530a367b6c2b857dae261cdbb67a440aaa")

;;; Core packages
(package! clojure-mode :pin "467922210962061777e00d6cc46f5db2d00fb3ef")
(when (and (modulep! +tree-sitter)
           (treesit-available-p)
           (> emacs-major-version 29))  ; requires 30+
  (package! clojure-ts-mode :pin "96fdffcbe9e1b8ebf9ad14e23b06f62cc3422e22"))
(package! clj-refactor :pin "362cb46bf808dc42d2aaf022afe93048439680c4")
(package! cider :pin "15bd3b02658cb8789ad9725fce829c3218bf77a9")
(when (modulep! :checkers syntax -flymake)
  (package! flycheck-clj-kondo :pin "e38c67ba9db1ea1cbe1b61ab39b506c05efdcdbf"))
(package! jet :pin "c9a92675efd802f37df5e3eab7858dbbeced6ea4")
(package! neil
  :recipe (:host github :repo "babashka/neil" :files ("*.el"))
  :pin "8d5ccdbd8185fb2e03bd7de04b3d190e90331709")
