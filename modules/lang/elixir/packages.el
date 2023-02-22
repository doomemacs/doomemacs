;; -*- no-byte-compile: t; -*-
;;; lang/elixir/packages.el

;; +elixir.el
(package! elixir-mode :pin "7641373f0563cab67cc5459c34534a8176b5e676")
(package! alchemist :pin "6f99367511ae209f8fe2c990779764bbb4ccb6ed")
(package! exunit :pin "e0a8c2b81f3d53885ed753b911b3cb6ee9229bec")
(when (modulep! :checkers syntax)
  (package! flycheck-credo :pin "e88f11ead53805c361ec7706e44c3dfee1daa19f"))
