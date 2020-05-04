;; -*- no-byte-compile: t; -*-
;;; lang/javascript/packages.el

;; Major modes
(package! coffee-mode :pin "35a41c7d8233eac0b267d9593e67fb8b6235e134")
(package! js2-mode :pin "fe53814dc2a0db2e95ac06083362e43923bf83fc")
(package! rjsx-mode :pin "0061587a06cdc2579a8d0e90863498d96bf982d8")
(package! typescript-mode :pin "102587e458d48ece6335cd708300647f22ec8b8d")

;; Tools
(package! js2-refactor :pin "d4c40b5fc86d3edd7c6a7d83ac86483ee1cb7a28")
(package! npm-mode :pin "3ee7c0bad5b7a041d4739ef3aaa06a3dc764e5eb")
(package! add-node-modules-path :pin "f31e69ccb681f882aebb806ce6e9478e3ac39708")

;; Eval
(package! nodejs-repl :pin "6fad7d764fa0d818ba497450bd722ae10cb8efed")
(package! skewer-mode :pin "e5bed351939c92a1f788f78398583c2f83f1bb3c")

;; Programming environment
(package! tide :pin "3b45610faaab33bc53ae2d44e1e573f19f35a74a")
(when (featurep! :tools lookup)
  (package! xref-js2 :pin "6f1ed5dae0c2485416196a51f2fa92f32e4b8262"))
