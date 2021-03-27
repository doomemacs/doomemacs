;; -*- no-byte-compile: t; -*-
;;; lang/ocaml/packages.el

(package! tuareg :pin "37a673020152ae0dbcaa250118b155d84e448f68")

(unless (featurep! +lsp)
  (package! merlin :pin "cc17ed60630fb1831ad950fe62970b1c7a7f4c8b")
  (package! merlin-eldoc :pin "db7fab1eddfe34781b7e79694f8923b285698032")
  (when (featurep! :checkers syntax)
    (package! flycheck-ocaml :pin "8707a7bf545a8639a6a5c600a98d9a2ea1487dc9")))

(package! ocp-indent :pin "9e26c0a2699b7076cebc04ece59fb354eb84c11c")

(when (featurep! :tools eval)
  (package! utop :pin "a5ff52bbf608e1112b5c0d41a36e3267f39f4084"))

(when (featurep! :editor format)
  (package! ocamlformat
    :recipe (:host github :repo "ocaml-ppx/ocamlformat" :files ("emacs/*.el"))
    :pin "448ac7c1496e2a6e83d63a7bcd9cf4e35bb2b1fb"))

(package! dune
  :recipe (:host github :repo "ocaml/dune" :files ("editor-integration/emacs/*.el"))
  :pin "a88ce5bbc996b550071c8df890276eb1b10e778c")
