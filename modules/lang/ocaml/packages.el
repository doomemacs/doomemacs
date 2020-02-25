;; -*- no-byte-compile: t; -*-
;;; lang/ocaml/packages.el

(package! tuareg :pin "c12061eb80")

(unless (featurep! +lsp)
  (package! merlin :pin "f6954e953b")
  (package! merlin-eldoc :pin "db7fab1edd")
  (when (featurep! :checkers syntax)
    (package! flycheck-ocaml :pin "8707a7bf54")))

(package! ocp-indent :pin "9e26c0a269")

(when (featurep! :tools eval)
  (package! utop :pin "7c99d8c904"))

(when (featurep! :editor format)
  ;; by default quelpa generated a version 0pre0.20180929.192844, which got
  ;; parsed into (0 -1 0 ...), which when compared with version nil (0) in
  ;; package-installed-p always yielded false
  (package! ocamlformat :recipe
    (:host github :repo "ocaml-ppx/ocamlformat" :files ("emacs/*.el")) :pin "dba4487820"))

(package! dune :recipe
  (:host github :repo "ocaml/dune" :files ("editor-integration/emacs/*.el")) :pin "f3df7abe64")
