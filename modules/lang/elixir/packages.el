;; -*- no-byte-compile: t; -*-
;;; lang/elixir/packages.el

;; +elixir.el
(package! elixir-mode :pin "00d6580a040a750e019218f9392cf9a4c2dac23a")
(package! exunit :pin "ee06b14b61beaf59d847802b83b027ac991f3496")
(when (modulep! :checkers syntax -flymake)
  (package! flycheck-credo :pin "e285bd042a535d0f13e0b4c5226df404cdda4033"))
