;; -*- no-byte-compile: t; -*-
;;; lang/csharp/packages.el

(package! csharp-mode :pin "57bd21bda4edc16671a85c7d6d51484e40a6e640")

(unless (featurep! +lsp)
  (package! omnisharp :pin "e658a18a762438c3e1737612737b05d02a21ca2a"))

(when (featurep! +unity)
  (package! shader-mode :pin "d7dc8d0d6fe8914e8b6d5cf2081ad61e6952359c"))
