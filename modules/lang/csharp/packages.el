;; -*- no-byte-compile: t; -*-
;;; lang/csharp/packages.el

(package! csharp-mode :pin "9917e1b97d6a374c8043124817142ea3419a649b")
(package! csproj-mode :pin "a7f0f4610c976a28c41b9b8299892f88b5d0336c")
(package! sln-mode :pin "0f91d1b957c7d2a7bab9278ec57b54d57f1dbd9c")
(when (featurep! +unity)
  (package! shader-mode :pin "d7dc8d0d6fe8914e8b6d5cf2081ad61e6952359c"))
(when (featurep! +dotnet)
  (package! sharper :pin "08277b6c30568adfbe438c9f2a1d6c3db4b7ebeb"))
