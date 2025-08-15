;; -*- no-byte-compile: t; -*-
;;; lang/javascript/packages.el

;; Major modes
(package! rjsx-mode :pin "b697fe4d92cc84fa99a7bcb476f815935ea0d919")
(package! typescript-mode :pin "481df3ad2cdf569d8e6697679669ff6206fbd2f9")

;; Tools
(package! js2-refactor :pin "e1177c728ae52a5e67157fb18ee1409d8e95386a")
(package! npm-mode :pin "3ee7c0bad5b7a041d4739ef3aaa06a3dc764e5eb")

;; Eval
(package! nodejs-repl :pin "130d49b073a50b7aad472ae8cd05848a9840e480")
(package! skewer-mode :pin "e5bed351939c92a1f788f78398583c2f83f1bb3c")

;; Programming environment
(package! tide :pin "6a35fe355f1442da34b976bf2decf008d6e4f991")
(when (modulep! :tools lookup)
  (package! xref-js2 :pin "e215af9eedac69b40942fff9d5514704f9f4d43e"))
