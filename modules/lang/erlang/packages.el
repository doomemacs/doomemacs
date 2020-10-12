;; -*- no-byte-compile: t; -*-
;;; lang/erlang/packages.el

(package! erlang :pin "36b4ba407e6f3c70ff88f8e3321622372601f6f6")
(when (featurep! :checkers syntax)
  (package! flycheck-rebar3 :pin "3cca1268c54643204b5bae52e3f0bf5bc921018c"))
(unless (featurep! +lsp)
  (when (featurep! :completion company)
    (package! company-erlang :pin "bc0524a16f17b66c7397690e4ca0e004f09ea6c5")))
