;; -*- no-byte-compile: t; -*-
;;; lang/ocaml/packages.el

(package! tuareg :pin "f0cb55f2177f6fc978d98d018910fe5b1890fe0c")
(package! opam-switch-mode :pin "1069e56a662f23ea09d4e05611bdedeb99257012")
(package! ocp-indent :pin "e2ac9daae2579151ad258b40071e5074a4b1c3fa")
(package! dune
  :recipe (:host github :repo "ocaml/dune" :files ("editor-integration/emacs/*.el"))
  :pin "14df34d30d51bcbda475c92e8dbdb392f79b3149")

(unless (modulep! +lsp)
  (package! merlin :pin "a0b096c243bbcea483bf8728a71c30eeafb18d11")
  (package! merlin-eldoc :pin "bf8edc63d85b35e4def352fa7ce4ea39f43e1fd8")
  (package! merlin-company :pin "a0b096c243bbcea483bf8728a71c30eeafb18d11")
  (when (modulep! :checkers syntax -flymake)
    (package! flycheck-ocaml :pin "77f8ddbd9bfc3a11957ac7ec7e45d5fa9179b192")))

(when (modulep! :tools eval)
  (package! utop :pin "33201e60767fec2b4d7f43d5f033ec1e961ac1ff"))
