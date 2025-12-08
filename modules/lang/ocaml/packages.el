;; -*- no-byte-compile: t; -*-
;;; lang/ocaml/packages.el

(package! tuareg :pin "f0cb55f2177f6fc978d98d018910fe5b1890fe0c")
(package! opam-switch-mode :pin "1069e56a662f23ea09d4e05611bdedeb99257012")
(package! ocp-indent :pin "e2ac9daae2579151ad258b40071e5074a4b1c3fa")
(package! dune
  :recipe (:host github :repo "ocaml/dune" :files ("editor-integration/emacs/*.el"))
  :pin "a40c461736ac7a6817de197504ee1ecfbca9e8cd")

(unless (modulep! +lsp)
  (package! merlin :pin "ecfbed397691c168ab23a4dede3cc2f5048251ae")
  (package! merlin-eldoc :pin "bf8edc63d85b35e4def352fa7ce4ea39f43e1fd8")
  (package! merlin-company :pin "ecfbed397691c168ab23a4dede3cc2f5048251ae")
  (when (modulep! :checkers syntax -flymake)
    (package! flycheck-ocaml :pin "77f8ddbd9bfc3a11957ac7ec7e45d5fa9179b192")))

(when (modulep! :tools eval)
  (package! utop :pin "33201e60767fec2b4d7f43d5f033ec1e961ac1ff"))
