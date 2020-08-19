;; -*- no-byte-compile: t; -*-
;;; lang/erlang/packages.el

(package! erlang :pin "d9bc7858e985143a433953ba286422511b466a0c")
(when (featurep! :checkers syntax)
  (package! flycheck-rebar3 :pin "3cca1268c54643204b5bae52e3f0bf5bc921018c"))
(unless (featurep! +lsp)
  (when (featurep! :completion company)
    (package! company-erlang :pin "bc0524a16f17b66c7397690e4ca0e004f09ea6c5")))
