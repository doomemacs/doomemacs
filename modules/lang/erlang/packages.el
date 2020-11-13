;; -*- no-byte-compile: t; -*-
;;; lang/erlang/packages.el

(package! erlang :pin "36b4ba407e6f3c70ff88f8e3321622372601f6f6")
(unless (featurep! +lsp)
  (when (featurep! :completion company)
    (package! company-erlang :pin "bc0524a16f17b66c7397690e4ca0e004f09ea6c5")))
