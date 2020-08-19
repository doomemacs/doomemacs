;; -*- no-byte-compile: t; -*-
;;; lang/elixir/packages.el

;; +elixir.el
(package! elixir-mode :pin "01b332495d3f44addeb236428041c4ffa0c2ca3b")
(package! alchemist :pin "6f99367511ae209f8fe2c990779764bbb4ccb6ed")
(when (featurep! :checkers syntax)
  (package! flycheck-credo :pin "e88f11ead53805c361ec7706e44c3dfee1daa19f"))
