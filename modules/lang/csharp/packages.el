;; -*- no-byte-compile: t; -*-
;;; lang/csharp/packages.el

(package! csharp-mode :pin "09b4d57d020a97e1696e89baa6d9855ff5c86014")
(package! csproj-mode :pin "a7f0f4610c976a28c41b9b8299892f88b5d0336c")
(package! sln-mode :pin "0f91d1b957c7d2a7bab9278ec57b54d57f1dbd9c")
(unless (featurep! +lsp)
  (package! omnisharp :pin "8ec59296b2e2a12d8ea146fb7056091fbb7bdd10"))
(when (featurep! +unity)
  (package! shader-mode :pin "d7dc8d0d6fe8914e8b6d5cf2081ad61e6952359c"))
(when (featurep! +dotnet)
  (package! sharper :pin "d610b839dbb907cc0a49b7edfe7fe39aa3f9dd6d"))
