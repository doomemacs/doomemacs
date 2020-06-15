;; -*- no-byte-compile: t; -*-
;;; lang/elixir/packages.el

;; +elixir.el
(package! elixir-mode :pin "670098a")
(package! alchemist :pin "6f99367511")
(when (featurep! :checkers syntax)
  (package! flycheck-credo :pin "e88f11ead5"))
