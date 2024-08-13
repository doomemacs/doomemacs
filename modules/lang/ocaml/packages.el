;; -*- no-byte-compile: t; -*-
;;; lang/ocaml/packages.el

(package! tuareg :pin "1d53723e39f22ab4ab76d31f2b188a2879305092")
(package! opam-switch-mode :pin "1069e56a662f23ea09d4e05611bdedeb99257012")
(package! ocp-indent :pin "f38578c25d62701847b1bcb45099a9020e2032fe")
(package! dune
  :recipe (:host github :repo "ocaml/dune" :files ("editor-integration/emacs/*.el"))
  :pin "96ed5fb42f2c534578cb77191887f8d5a7bf5529")

(unless (modulep! +lsp)
  (package! merlin :pin "9fa77dbe81c893476646d873c5ac5106b94b7107")
  (package! merlin-eldoc :pin "bf8edc63d85b35e4def352fa7ce4ea39f43e1fd8")
  (package! merlin-company :pin "9fa77dbe81c893476646d873c5ac5106b94b7107")
  (when (and (modulep! :checkers syntax)
             (not (modulep! :checkers syntax +flymake)))
    (package! flycheck-ocaml :pin "77f8ddbd9bfc3a11957ac7ec7e45d5fa9179b192")))

(when (modulep! :tools eval)
  (package! utop :pin "384b3098c8c4a2e26b87167053952b753aa8a63a"))

