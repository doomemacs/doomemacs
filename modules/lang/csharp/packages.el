;; -*- no-byte-compile: t; -*-
;;; lang/csharp/packages.el

(package! csharp-mode :pin "856ecbc0a78ae3bdc2db2ae4d16be43e2d9d9c5e")
(package! csproj-mode :pin "a7f0f4610c976a28c41b9b8299892f88b5d0336c")
(package! sln-mode :pin "0f91d1b957c7d2a7bab9278ec57b54d57f1dbd9c")
(when (featurep! +unity)
  (package! shader-mode :pin "d7dc8d0d6fe8914e8b6d5cf2081ad61e6952359c"))
(when (featurep! +dotnet)
  (package! sharper :pin "96edd4a1dbc267afdff0cb97298d1b05b7c2080c"))
