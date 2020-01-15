;; -*- no-byte-compile: t; -*-
;;; private/erlang/packages.el

(package! erlang :pin "cf6cf5e5f82e348ecb9bb02d70027fc4961aee3d")
(when (featurep! :checkers syntax)
  (package! flycheck-rebar3 :pin "3cca1268c54643204b5bae52e3f0bf5bc921018c"))
(unless (featurep! +lsp)
  (when (featurep! :completion ivy)
    (package! ivy-erlang-complete :pin "7d60ed111dbfd34ab6ec1b07c06e2d16a5380b9a"))
  (when (featurep! :completion company)
    (package! company-erlang :pin "bc0524a16f17b66c7397690e4ca0e004f09ea6c5")))
