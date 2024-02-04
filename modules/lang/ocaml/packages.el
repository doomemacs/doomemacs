;; -*- no-byte-compile: t; -*-
;;; lang/ocaml/packages.el

(package! tuareg :pin "1d53723e39f22ab4ab76d31f2b188a2879305092")

(unless (modulep! +lsp)
  (package! merlin :pin "8404f96693727f7b0edc0d0b14db57843d79e27b")
  (package! merlin-eldoc :pin "bf8edc63d85b35e4def352fa7ce4ea39f43e1fd8")
  (package! merlin-company :pin "8404f96693727f7b0edc0d0b14db57843d79e27b")
  (when (and (modulep! :checkers syntax)
             (not (modulep! :checkers syntax +flymake)))
    (package! flycheck-ocaml :pin "77f8ddbd9bfc3a11957ac7ec7e45d5fa9179b192")))

(package! ocp-indent :pin "f38578c25d62701847b1bcb45099a9020e2032fe")

(when (modulep! :tools eval)
  (package! utop :pin "8cc563282597abdb8f5cca64df41166c5ebca6b5"))

(when (modulep! :editor format)
  (package! ocamlformat
    :recipe (:host github :repo "ocaml-ppx/ocamlformat" :files ("emacs/*.el"))
    :pin "7db948a8946e589edd11334dda56505f8d826dca"))

(package! dune
  :recipe (:host github :repo "ocaml/dune" :files ("editor-integration/emacs/*.el"))
  :pin "360b00027166473a01a9010a04fc30af3ee883a0")
