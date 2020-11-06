;; -*- no-byte-compile: t; -*-
;;; lang/csharp/packages.el

(package! csharp-mode :pin "f46d656fc4ceefeb0ed8f5df8baaf0809a7a495d")
(package! csproj-mode :pin "a7f0f4610c976a28c41b9b8299892f88b5d0336c")
(unless (featurep! +lsp)
  (package! omnisharp :pin "e26ff8b8d34a247cd4a93be5d62a5f21859b7b57"))
(when (featurep! +unity)
  (package! shader-mode :pin "d7dc8d0d6fe8914e8b6d5cf2081ad61e6952359c"))
