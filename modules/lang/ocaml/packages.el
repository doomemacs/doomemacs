;; -*- no-byte-compile: t; -*-
;;; lang/ocaml/packages.el

(package! tuareg :pin "00faf47a7c65e4cdcf040f38add1c6a08cd2ee2f")

(unless (featurep! +lsp)
  (package! merlin :pin "e4791e22986993c36c3f5c91e8dff93494cc232e")
  (package! merlin-eldoc :pin "db7fab1eddfe34781b7e79694f8923b285698032")
  (package! merlin-company :pin "e4791e22986993c36c3f5c91e8dff93494cc232e")
  (when (featurep! :checkers syntax)
    (package! flycheck-ocaml :pin "8707a7bf545a8639a6a5c600a98d9a2ea1487dc9")))

(package! ocp-indent :pin "9e26c0a2699b7076cebc04ece59fb354eb84c11c")

(when (featurep! :tools eval)
  (package! utop :pin "c87b8b2817eefd0cd53564618911386b89b587c5"))

(when (featurep! :editor format)
  (package! ocamlformat
    :recipe (:host github :repo "ocaml-ppx/ocamlformat" :files ("emacs/*.el"))
    :pin "22a3707da387f9ab82b42a87c358133b829f2222"))

(package! dune
  :recipe (:host github :repo "ocaml/dune" :files ("editor-integration/emacs/*.el"))
  :pin "4d0a47edd53dc2141059cbf99bfec858a08e9fe0")
