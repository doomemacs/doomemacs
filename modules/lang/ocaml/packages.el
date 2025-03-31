;; -*- no-byte-compile: t; -*-
;;; lang/ocaml/packages.el

(package! tuareg :pin "1600fdad28bdd2c55e52a87e7987713c6d5d1718")
(package! opam-switch-mode :pin "1069e56a662f23ea09d4e05611bdedeb99257012")
(package! ocp-indent :pin "f38578c25d62701847b1bcb45099a9020e2032fe")
(package! dune
  :recipe (:host github :repo "ocaml/dune" :files ("editor-integration/emacs/*.el"))
  :pin "a46193e1679ab7e1d7c816a66c6752a790487f4c")

(unless (modulep! +lsp)
  (package! merlin :pin "2eeb63d0026ab8a18b9e31ea61729af513dcb5d6")
  (package! merlin-eldoc :pin "bf8edc63d85b35e4def352fa7ce4ea39f43e1fd8")
  (package! merlin-company :pin "2eeb63d0026ab8a18b9e31ea61729af513dcb5d6")
  (when (modulep! :checkers syntax -flymake)
    (package! flycheck-ocaml :pin "77f8ddbd9bfc3a11957ac7ec7e45d5fa9179b192")))

(when (modulep! :tools eval)
  (package! utop :pin "3322adaa5267b1188d14b15e85c802c21fe061cb"))
