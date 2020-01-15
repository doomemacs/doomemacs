;; -*- no-byte-compile: t; -*-
;;; private/erlang/packages.el

(package! erlang)

(when (featurep! :checkers syntax)
  (package! flycheck-rebar3))

(when (and (featurep! :completion ivy)
           (not (featurep! +lsp)))
  (package! ivy-erlang-complete))

(when (and (featurep! :completion company)
           (not (featurep! +lsp)))
  (package! company-erlang))
