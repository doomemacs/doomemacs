;; -*- no-byte-compile: t; -*-
;;; lang/elixir/packages.el

;; +elixir.el
(package! elixir-mode :pin "6bbc1e5ac46064613c982cedc60566ed077e7a58")
(package! alchemist :pin "6f99367511ae209f8fe2c990779764bbb4ccb6ed")
(package! exunit :pin "5bb115f3270cfe29d36286da889f0ee5bba03cfd")
(when (featurep! :checkers syntax)
  (package! flycheck-credo :pin "e88f11ead53805c361ec7706e44c3dfee1daa19f"))
