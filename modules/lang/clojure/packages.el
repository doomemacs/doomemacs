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
(package! parseclj :pin "90595049634549e6d8872f719b13e9555897d17b")
(package! parseedn :pin "ea7b5281ec80aca0bd1cc93a348aebb302497339")

;;; Core packages
(package! clojure-mode :pin "b7d08b87f6a116ff47b33ee857926b60c66c3ab7")
(package! clj-refactor :pin "f368c56c83843396b160440f472a661a3b639862")
(package! cider :pin "02ca53021682f426323dc7bb4e6b28e6c8f5eb30")
(when (featurep! :checkers syntax)
  (package! flycheck-clj-kondo :pin "d8a6ee9a16aa24b5be01f1edf9843d41bdc75555"))
