;; -*- no-byte-compile: t; -*-
;;; lang/elixir/packages.el

;; +elixir.el
(package! elixir-mode :pin "5920edcf19f0526bbee97b01435c4b8bf3b59c36")
(package! alchemist :pin "6f99367511ae209f8fe2c990779764bbb4ccb6ed")
(when (featurep! :checkers syntax)
  (package! flycheck-credo :pin "e88f11ead53805c361ec7706e44c3dfee1daa19f"))
