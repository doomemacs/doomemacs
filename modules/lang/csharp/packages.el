;; -*- no-byte-compile: t; -*-
;;; lang/csharp/packages.el

(package! csharp-mode :pin "57bd21bda4")

(unless (featurep! +lsp)
  (package! omnisharp :pin "e658a18a76"))

(when (featurep! +unity)
  (package! shader-mode :pin "d7dc8d0d6f"))
