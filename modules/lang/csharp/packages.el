;; -*- no-byte-compile: t; -*-
;;; lang/csharp/packages.el

(package! csharp-mode
  :pin "02c61c219b2c22491eff9b7315fed661fab423d4"
  :built-in 'prefer)  ; Built-in as of Emacs 29

(package! csproj-mode :pin "a7f0f4610c976a28c41b9b8299892f88b5d0336c")
(package! sln-mode :pin "0f91d1b957c7d2a7bab9278ec57b54d57f1dbd9c")
;; sln-mode depends on font-lock-ext
(package! font-lock-ext :pin "b6c82e8ac7996d96494a54454015a98ceb883feb")
(when (modulep! +unity)
  (package! shader-mode :pin "fe5a1982ba69e4a98b834141a46a1908f132df15"))
(when (modulep! +dotnet)
  (package! sharper :pin "5049795848609e6508e4c9718a9f97ee481bf36c"))
