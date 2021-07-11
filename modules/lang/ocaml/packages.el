;; -*- no-byte-compile: t; -*-
;;; lang/ocaml/packages.el

(package! tuareg :pin "b59c422759506402f990b089dbaa91c0578e2c2e")

(unless (featurep! +lsp)
  (package! merlin :pin "5731826810ef8caa2201d8e1f4385ce83f99c909")
  (package! merlin-eldoc :pin "db7fab1eddfe34781b7e79694f8923b285698032")
  (package! merlin-company :pin "5731826810ef8caa2201d8e1f4385ce83f99c909")
  (when (featurep! :checkers syntax)
    (package! flycheck-ocaml :pin "8707a7bf545a8639a6a5c600a98d9a2ea1487dc9")))

(package! ocp-indent :pin "9e26c0a2699b7076cebc04ece59fb354eb84c11c")

(when (featurep! :tools eval)
  (package! utop :pin "c87b8b2817eefd0cd53564618911386b89b587c5"))

(when (featurep! :editor format)
  (package! ocamlformat
    :recipe (:host github :repo "ocaml-ppx/ocamlformat" :files ("emacs/*.el"))
    :pin "5dd6574d8fed4f7c6b76aab2d8dea9886c3642ee"))

(package! dune
  :recipe (:host github :repo "ocaml/dune" :files ("editor-integration/emacs/*.el"))
  :pin "75ecfe34216dd07b339d2b9027f7b6a507151418")
