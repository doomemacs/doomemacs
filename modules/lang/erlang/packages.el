;; -*- no-byte-compile: t; -*-
;;; private/erlang/packages.el

(package! erlang :pin "3065fbf434")
(when (featurep! :checkers syntax)
  (package! flycheck-rebar3 :pin "3cca1268c5"))
(unless (featurep! +lsp)
  (when (featurep! :completion company)
    (package! company-erlang :pin "bc0524a16f")))
