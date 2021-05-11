;; -*- no-byte-compile: t; -*-
;;; lang/ocaml/packages.el

(package! tuareg :pin "37a673020152ae0dbcaa250118b155d84e448f68")

(unless (featurep! +lsp)
  (package! merlin :pin "635923da0771cc0cb7154d3fc58e348e9148766d")
  (package! merlin-eldoc :pin "db7fab1eddfe34781b7e79694f8923b285698032")
  (package! merlin-company :pin "635923da0771cc0cb7154d3fc58e348e9148766d")
  (when (featurep! :checkers syntax)
    (package! flycheck-ocaml :pin "8707a7bf545a8639a6a5c600a98d9a2ea1487dc9")))

(package! ocp-indent :pin "9e26c0a2699b7076cebc04ece59fb354eb84c11c")

(when (featurep! :tools eval)
  (package! utop :pin "711c24661ce625070f8981fab9c6f31ce72b0a52"))

(when (featurep! :editor format)
  (package! ocamlformat
    :recipe (:host github :repo "ocaml-ppx/ocamlformat" :files ("emacs/*.el"))
    :pin "0ad8d0a5a55e28e425fdc89e220274447500f0d2"))

(package! dune
  :recipe (:host github :repo "ocaml/dune" :files ("editor-integration/emacs/*.el"))
  :pin "65e04ba5646e8ba4a033b099c92fbda9b9aca341")
