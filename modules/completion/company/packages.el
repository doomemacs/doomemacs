;; -*- no-byte-compile: t; -*-
;;; completion/company/packages.el

(package! company :pin "5f5949b6ae8ea9df94c6cb3e01d9cae43623b794")
(package! company-dict :pin "cd7b8394f6014c57897f65d335d6b2bd65dab1f4")
(when (featurep! +childframe)
  (package! company-box :pin "be37a9a30dc112ab172af21af694e2cb04a74f85"))
