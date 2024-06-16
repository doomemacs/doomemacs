;; -*- no-byte-compile: t; -*-
;;; lang/elixir/packages.el

;; +elixir.el
(package! elixir-mode :pin "00d6580a040a750e019218f9392cf9a4c2dac23a")
(package! exunit :pin "e008c89e01e5680473278c7e7bab42842e294e4d")
(when (and (modulep! :checkers syntax)
           (not (modulep! :checkers syntax +flymake)))
  (package! flycheck-credo :pin "e285bd042a535d0f13e0b4c5226df404cdda4033"))
