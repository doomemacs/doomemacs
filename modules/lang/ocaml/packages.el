;; -*- no-byte-compile: t; -*-
;;; lang/ocaml/packages.el

(package! tuareg :pin "ccde45bbc292123ec20617f1af7f7e19f7481545")

(unless (featurep! +lsp)
  (package! merlin :pin "36d0aefcecdd69e201f69a55b53b51709cdaf6b4")
  (package! merlin-eldoc :pin "db7fab1eddfe34781b7e79694f8923b285698032")
  (when (featurep! :checkers syntax)
    (package! flycheck-ocaml :pin "8707a7bf545a8639a6a5c600a98d9a2ea1487dc9")))

(package! ocp-indent :pin "9e26c0a2699b7076cebc04ece59fb354eb84c11c")

(when (featurep! :tools eval)
  (package! utop :pin "a5ff52bbf608e1112b5c0d41a36e3267f39f4084"))

(when (featurep! :editor format)
  (package! ocamlformat
    :recipe (:host github :repo "ocaml-ppx/ocamlformat" :files ("emacs/*.el"))
    :pin "7db8d1377a127b39e2bf985c91b6a9a8d6cbeff2"))

(package! dune
  :recipe (:host github :repo "ocaml/dune" :files ("editor-integration/emacs/*.el"))
  :pin "f839fc18b512a74bcf8e0d76a96bb078bd0fb601")
