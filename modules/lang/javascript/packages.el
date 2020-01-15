;; -*- no-byte-compile: t; -*-
;;; lang/javascript/packages.el

;; Major modes
(package! coffee-mode :pin "86ab8aae8662e8eff54d3013010b9c693b16eac5")
(package! js2-mode :pin "b3841a7a304d9d1328fdb0868fbbecf0c2f9831f")
(package! rjsx-mode :pin "0e7fa6b4facdec4f85a7a8865bdf59dfd57217b5")
(package! typescript-mode :pin "761f3aec6e192ddf0a9f1cc3d5d2ee77d32cb06c")

;; Tools
(package! eslintd-fix :pin "98c669e3653bf94c236c54946c6faba7f782ef0d")
(package! js2-refactor :pin "d4c40b5fc86d3edd7c6a7d83ac86483ee1cb7a28")
(package! npm-mode :pin "3ee7c0bad5b7a041d4739ef3aaa06a3dc764e5eb")

;; Eval
(package! nodejs-repl :pin "8b9094826568485eb0c48d798ae0026cb6962b83")
(package! skewer-mode :pin "123215dd9bfa67ce5cc49cd52dd54c0ba7c7e02c")

;; Programming environment
(package! tide :pin "1878a097fc41ee81c40c155022c8feaaf8bfaa6d")
(when (featurep! :tools lookup)
  (package! xref-js2 :pin "6f1ed5dae0c2485416196a51f2fa92f32e4b8262"))
