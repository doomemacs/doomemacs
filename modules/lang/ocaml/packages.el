;; -*- no-byte-compile: t; -*-
;;; lang/ocaml/packages.el

(package! tuareg :pin "f0cb55f2177f6fc978d98d018910fe5b1890fe0c")
(package! opam-switch-mode :pin "1069e56a662f23ea09d4e05611bdedeb99257012")
(package! ocp-indent :pin "e2ac9daae2579151ad258b40071e5074a4b1c3fa")
(package! dune
  :recipe (:host github :repo "ocaml/dune" :files ("editor-integration/emacs/*.el"))
  :pin "466de64531cd16aec04e632cdec78462511a8729")

(unless (modulep! +lsp)
  (package! merlin :pin "eb413b238b169d75ceb79a7dd37dc01e6cc73d9b")
  (package! merlin-eldoc :pin "bf8edc63d85b35e4def352fa7ce4ea39f43e1fd8")
  (package! merlin-company :pin "eb413b238b169d75ceb79a7dd37dc01e6cc73d9b")
  (when (modulep! :checkers syntax -flymake)
    (package! flycheck-ocaml :pin "77f8ddbd9bfc3a11957ac7ec7e45d5fa9179b192")))

(when (modulep! :tools eval)
  (package! utop :pin "8cd6716d0f90efd67da90afa5bc31c7abe9a7a57"))
