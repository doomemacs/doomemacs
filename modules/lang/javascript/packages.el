;; -*- no-byte-compile: t; -*-
;;; lang/javascript/packages.el

;; Major modes
(package! rjsx-mode :pin "b697fe4d92cc84fa99a7bcb476f815935ea0d919")
(package! typescript-mode :pin "fc3a4f3b275e8cf6cf41aa0c9ef42e25ef908feb")

;; Tools
(package! js2-refactor :pin "a0977c4ce1918cc266db9d6cd7a2ab63f3a76b9a")
(package! npm-mode :pin "3ee7c0bad5b7a041d4739ef3aaa06a3dc764e5eb")

;; Eval
(package! nodejs-repl :pin "03d0b64768b40b71ae54de1b50eb58719e615746")
(package! skewer-mode :pin "e5bed351939c92a1f788f78398583c2f83f1bb3c")

;; Programming environment
(package! tide :pin "b38dfc3f8fb754e64e48e76fc92d472cb3d1a3dc")
(when (modulep! :tools lookup)
  (package! xref-js2 :pin "e215af9eedac69b40942fff9d5514704f9f4d43e"))
