;; -*- no-byte-compile: t; -*-
;;; lang/ocaml/packages.el

(package! tuareg :pin "1d53723e39f22ab4ab76d31f2b188a2879305092")
(package! opam-switch-mode :pin "1069e56a662f23ea09d4e05611bdedeb99257012")
(package! ocp-indent :pin "f38578c25d62701847b1bcb45099a9020e2032fe")
(package! dune
  :recipe (:host github :repo "ocaml/dune" :files ("editor-integration/emacs/*.el"))
  :pin "a7924e322e724d9e604eb0c6f5fd9712d4dee3a0")

(unless (modulep! +lsp)
  (package! merlin :pin "e016abfac6da88ff7a6b49a42ca37854a4378ace")
  (package! merlin-eldoc :pin "bf8edc63d85b35e4def352fa7ce4ea39f43e1fd8")
  (package! merlin-company :pin "e016abfac6da88ff7a6b49a42ca37854a4378ace")
  (when (modulep! :checkers syntax -flymake)
    (package! flycheck-ocaml :pin "77f8ddbd9bfc3a11957ac7ec7e45d5fa9179b192")))

(when (modulep! :tools eval)
  (package! utop :pin "384b3098c8c4a2e26b87167053952b753aa8a63a"))

