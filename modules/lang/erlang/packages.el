;; -*- no-byte-compile: t; -*-
;;; private/erlang/packages.el

(package! erlang)

(when (featurep! :feature syntax-checker)
  (package! flycheck-rebar3))

(when (featurep! :completion ivy)
  (package! ivy-erlang-complete))

(when (featurep! :completion company)
  (package! company-erlang))
