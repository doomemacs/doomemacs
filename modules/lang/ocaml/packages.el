;; -*- no-byte-compile: t; -*-
;;; lang/ocaml/packages.el

(package! tuareg :pin "53ce2fdfdd372d52f3a6547c33b687e7d403357a")

(unless (modulep! +lsp)
  (package! merlin :pin "306af713e268d810ea0dc80bdf8e98265f96bd51")
  (package! merlin-eldoc :pin "bf8edc63d85b35e4def352fa7ce4ea39f43e1fd8")
  (package! merlin-company :pin "306af713e268d810ea0dc80bdf8e98265f96bd51")
  (when (modulep! :checkers syntax)
    (package! flycheck-ocaml :pin "77f8ddbd9bfc3a11957ac7ec7e45d5fa9179b192")))

(package! ocp-indent :pin "7c4d434132cebc15a8213c8be9e7323692eb0a2b")

(when (modulep! :tools eval)
  (package! utop :pin "ace481388a54fdf89244a76497fbdedb4ff15207"))

(when (modulep! :editor format)
  (package! ocamlformat
    :recipe (:host github :repo "ocaml-ppx/ocamlformat" :files ("emacs/*.el"))
    :pin "1ad4bdba9e1fd4a0bd73e22f75b3079c70cdba53"))

(package! dune
  :recipe (:host github :repo "ocaml/dune" :files ("editor-integration/emacs/*.el"))
  :pin "f5a5a9c17be60b1cf2ec22fee35d35ccc3bb5e5e")
