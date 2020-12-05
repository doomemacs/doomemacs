;; -*- no-byte-compile: t; -*-
;;; lang/erlang/packages.el

(package! erlang :pin "8fc06415329c72a1e2b8293e98b0317d28152ce0")
(unless (featurep! +lsp)
  (when (featurep! :completion company)
    (package! company-erlang :pin "bc0524a16f17b66c7397690e4ca0e004f09ea6c5")))
