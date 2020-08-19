;; -*- no-byte-compile: t; -*-
;;; lang/ocaml/packages.el

(package! tuareg :pin "ccde45bbc292123ec20617f1af7f7e19f7481545")

(unless (featurep! +lsp)
  (package! merlin :pin "3751cbfff75022c396c4ff4dc1729048f80daa4f")
  (package! merlin-eldoc :pin "db7fab1eddfe34781b7e79694f8923b285698032")
  (when (featurep! :checkers syntax)
    (package! flycheck-ocaml :pin "8707a7bf545a8639a6a5c600a98d9a2ea1487dc9")))

(package! ocp-indent :pin "9e26c0a2699b7076cebc04ece59fb354eb84c11c")

(when (featurep! :tools eval)
  (package! utop :pin "7bc5117d3449fc19f5c706a6decfdb2a30984507"))

(when (featurep! :editor format)
  (package! ocamlformat
    :recipe (:host github :repo "ocaml-ppx/ocamlformat" :files ("emacs/*.el"))
    :pin "cd0eaa8bbb7df431276cf65741c53eaa913f7807"))

(package! dune
  :recipe (:host github :repo "ocaml/dune" :files ("editor-integration/emacs/*.el"))
  :pin "daea74e5366c8571af8674860a733b0c9e1eaf81")
