;; -*- no-byte-compile: t; -*-
;;; lang/ocaml/packages.el

(package! tuareg :pin "c12061eb80")

(unless (featurep! +lsp)
  (package! merlin :pin "37e38e44f5")
  (package! merlin-eldoc :pin "db7fab1edd")
  (when (featurep! :checkers syntax)
    (package! flycheck-ocaml :pin "8707a7bf54")))

(package! ocp-indent :pin "9e26c0a269")

(when (featurep! :tools eval)
  (package! utop :pin "30c77ce4d7"))

(when (featurep! :editor format)
  ;; by default quelpa generated a version 0pre0.20180929.192844, which got
  ;; parsed into (0 -1 0 ...), which when compared with version nil (0) in
  ;; package-installed-p always yielded false
  (package! ocamlformat :recipe
    (:host github :repo "ocaml-ppx/ocamlformat" :files ("emacs/*.el")) :pin "5282e047bb"))

(package! dune :recipe
  (:host github :repo "ocaml/dune" :files ("editor-integration/emacs/*.el")) :pin "1944d0fb52")
