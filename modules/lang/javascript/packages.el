;; -*- no-byte-compile: t; -*-
;;; lang/javascript/packages.el

;; Major modes
(package! coffee-mode :pin "35a41c7d8233eac0b267d9593e67fb8b6235e134")
(package! js2-mode :pin "29979e5f3301796ba606759e39ee0b1b6a2a24f3")
(package! rjsx-mode :pin "b697fe4d92cc84fa99a7bcb476f815935ea0d919")
(package! typescript-mode :pin "1043025d42602d560949955410d3afa2562130ee")

;; Tools
(package! js2-refactor :pin "d4c40b5fc86d3edd7c6a7d83ac86483ee1cb7a28")
(package! npm-mode :pin "3ee7c0bad5b7a041d4739ef3aaa06a3dc764e5eb")
(package! add-node-modules-path :pin "f31e69ccb681f882aebb806ce6e9478e3ac39708")

;; Eval
(package! nodejs-repl :pin "3b841055cad00f442e4a9159b1056f59411b6646")
(package! skewer-mode :pin "e5bed351939c92a1f788f78398583c2f83f1bb3c")

;; Programming environment
(package! tide :pin "7f7334b42a40dd3093b830e887c36cdb4ef40858")
(when (featurep! :tools lookup)
  (package! xref-js2 :pin "6f1ed5dae0c2485416196a51f2fa92f32e4b8262"))
