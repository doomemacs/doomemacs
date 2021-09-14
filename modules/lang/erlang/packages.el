;; -*- no-byte-compile: t; -*-
;;; lang/erlang/packages.el

(package! erlang :pin "118cb37bd5b9e9cb792f0463e46fdb04f151dcd5")
(unless (featurep! +lsp)
  (when (featurep! :completion company)
    (package! company-erlang :pin "bc0524a16f17b66c7397690e4ca0e004f09ea6c5")))
