;; -*- no-byte-compile: t; -*-
;;; lang/elixir/packages.el

;; +elixir.el
(package! elixir-mode :pin "b78e7f733be7bf2af35807d759e53b7d52323193")
(package! alchemist :pin "6f99367511ae209f8fe2c990779764bbb4ccb6ed")
(package! exunit :pin "c77b0397b80d772c98fcc34c9ab131a8350fbf40")
(when (featurep! :checkers syntax)
  (package! flycheck-credo :pin "e88f11ead53805c361ec7706e44c3dfee1daa19f"))
