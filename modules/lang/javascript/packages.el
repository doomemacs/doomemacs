;; -*- no-byte-compile: t; -*-
;;; lang/javascript/packages.el

;; Major modes
(package! coffee-mode :pin "35a41c7d8233eac0b267d9593e67fb8b6235e134")
(package! js2-mode :pin "5049e543b52099e6ea3e9bc915fc023d5a9b2644")
(package! rjsx-mode :pin "0061587a06cdc2579a8d0e90863498d96bf982d8")
(package! typescript-mode :pin "0fc729787007b5111f3584034af0f3ef2389098f")

;; Tools
(package! js2-refactor :pin "d4c40b5fc86d3edd7c6a7d83ac86483ee1cb7a28")
(package! npm-mode :pin "3ee7c0bad5b7a041d4739ef3aaa06a3dc764e5eb")
(package! add-node-modules-path :pin "f31e69ccb681f882aebb806ce6e9478e3ac39708")

;; Eval
(package! nodejs-repl :pin "f5ce3d5b7b4e0d06f6e9d4930d9ecc417633586b")
(package! skewer-mode :pin "e5bed351939c92a1f788f78398583c2f83f1bb3c")

;; Programming environment
(package! tide :pin "13e7af77b6867ffca6eecc5b3b3b5e315518f49c")
(when (featurep! :tools lookup)
  (package! xref-js2 :pin "6f1ed5dae0c2485416196a51f2fa92f32e4b8262"))
