;; -*- no-byte-compile: t; -*-
;;; lang/csharp/packages.el

(package! csharp-mode :pin "48851778e0f01a2b0395e054e418a1d8a1687a06")
(package! csproj-mode :pin "a7f0f4610c976a28c41b9b8299892f88b5d0336c")
(unless (featurep! +lsp)
  (package! omnisharp :pin "e658a18a762438c3e1737612737b05d02a21ca2a"))
(when (featurep! +unity)
  (package! shader-mode :pin "d7dc8d0d6fe8914e8b6d5cf2081ad61e6952359c"))
