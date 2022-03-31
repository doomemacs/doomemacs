;; -*- no-byte-compile: t; -*-
;;; lang/ocaml/packages.el

(package! tuareg :pin "04f5ab6be9ae1c594bab359819dbaf708ae57fda")

(unless (featurep! +lsp)
  (package! merlin :pin "5d59c7065938ea9c9b52f368b97a50bc8bf6d65b")
  (package! merlin-eldoc :pin "db7fab1eddfe34781b7e79694f8923b285698032")
  (package! merlin-company :pin "5d59c7065938ea9c9b52f368b97a50bc8bf6d65b")
  (when (featurep! :checkers syntax)
    (package! flycheck-ocaml :pin "8707a7bf545a8639a6a5c600a98d9a2ea1487dc9")))

(package! ocp-indent :pin "7c4d434132cebc15a8213c8be9e7323692eb0a2b")

(when (featurep! :tools eval)
  (package! utop :pin "5d72a0ab34bf621b2150e9e267ec108fb1c5899a"))

(when (featurep! :editor format)
  (package! ocamlformat
    :recipe (:host github :repo "ocaml-ppx/ocamlformat" :files ("emacs/*.el"))
    :pin "9324ea439a77b4f3a31e9302b97ce1812cf8f17d"))

(package! dune
  :recipe (:host github :repo "ocaml/dune" :files ("editor-integration/emacs/*.el"))
  :pin "4bc7629a5e767623f35f0a52c2d4d147e18cf7ac")
