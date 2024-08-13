;; -*- no-byte-compile: t; -*-
;;; lang/ocaml/packages.el

(package! tuareg :pin "1d53723e39f22ab4ab76d31f2b188a2879305092")
(package! opam-switch-mode :pin "1069e56a662f23ea09d4e05611bdedeb99257012")
(package! ocp-indent :pin "f38578c25d62701847b1bcb45099a9020e2032fe")
(package! dune
  :recipe (:host github :repo "ocaml/dune" :files ("editor-integration/emacs/*.el"))
  :pin "aac3d84f1d5abdf276d72be3dccac23bf99b3c7c")

(unless (modulep! +lsp)
  (package! merlin :pin "b6ff2d4d569c23dd8fa91639d26fb984e9411862")
  (package! merlin-eldoc :pin "bf8edc63d85b35e4def352fa7ce4ea39f43e1fd8")
  (package! merlin-company :pin "b6ff2d4d569c23dd8fa91639d26fb984e9411862")
  (when (and (modulep! :checkers syntax)
             (not (modulep! :checkers syntax +flymake)))
    (package! flycheck-ocaml :pin "77f8ddbd9bfc3a11957ac7ec7e45d5fa9179b192")))

(when (modulep! :tools eval)
  (package! utop :pin "384b3098c8c4a2e26b87167053952b753aa8a63a"))

