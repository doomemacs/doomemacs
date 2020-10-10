;; -*- no-byte-compile: t; -*-
;;; lang/ocaml/packages.el

(package! tuareg :pin "ccde45bbc292123ec20617f1af7f7e19f7481545")

(unless (featurep! +lsp)
  (package! merlin :pin "06833e9dab5d5bb5ece68f11eb15797b6cd00933")
  (package! merlin-eldoc :pin "db7fab1eddfe34781b7e79694f8923b285698032")
  (when (featurep! :checkers syntax)
    (package! flycheck-ocaml :pin "8707a7bf545a8639a6a5c600a98d9a2ea1487dc9")))

(package! ocp-indent :pin "9e26c0a2699b7076cebc04ece59fb354eb84c11c")

(when (featurep! :tools eval)
  (package! utop :pin "7bc5117d3449fc19f5c706a6decfdb2a30984507"))

(when (featurep! :editor format)
  (package! ocamlformat
    :recipe (:host github :repo "ocaml-ppx/ocamlformat" :files ("emacs/*.el"))
    :pin "1dec6c3ffb2572b21d43e99653cabdf0406e7eef"))

(package! dune
  :recipe (:host github :repo "ocaml/dune" :files ("editor-integration/emacs/*.el"))
  :pin "f967df6f51f690eab612cdeb458926a4bdd41e11")
