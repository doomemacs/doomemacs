;; -*- no-byte-compile: t; -*-
;;; lang/erlang/packages.el

(package! erlang :pin "c1ab4b5424be7504cfc3c4e87a2116b7731d8f2d")
(unless (featurep! +lsp)
  (when (featurep! :completion company)
    (package! company-erlang :pin "bc0524a16f17b66c7397690e4ca0e004f09ea6c5")))
