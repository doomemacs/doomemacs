;; -*- no-byte-compile: t; -*-
;;; lang/ocaml/packages.el

(package! tuareg :pin "1600fdad28bdd2c55e52a87e7987713c6d5d1718")
(package! opam-switch-mode :pin "1069e56a662f23ea09d4e05611bdedeb99257012")
(package! ocp-indent :pin "12138576832400d7fbe6938258f646ddab314fbd")
(package! dune
  :recipe (:host github :repo "ocaml/dune" :files ("editor-integration/emacs/*.el"))
  :pin "28b7e8f3b2563a6e2c9e8838ae774d1aa82c6d8d")

(unless (modulep! +lsp)
  (package! merlin :pin "8b88b89ee7431a23eaf95e4e02e45dc65595aa74")
  (package! merlin-eldoc :pin "bf8edc63d85b35e4def352fa7ce4ea39f43e1fd8")
  (package! merlin-company :pin "8b88b89ee7431a23eaf95e4e02e45dc65595aa74")
  (when (modulep! :checkers syntax -flymake)
    (package! flycheck-ocaml :pin "77f8ddbd9bfc3a11957ac7ec7e45d5fa9179b192")))

(when (modulep! :tools eval)
  (package! utop :pin "33201e60767fec2b4d7f43d5f033ec1e961ac1ff"))
