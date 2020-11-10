;; -*- no-byte-compile: t; -*-
;;; completion/company/packages.el

(package! company :pin "dd925936f7c0bf00319c81e8caea1b3db63bb8b5")
(package! company-dict :pin "cd7b8394f6014c57897f65d335d6b2bd65dab1f4")
(when (featurep! +childframe)
  (package! company-box :pin "be37a9a30dc112ab172af21af694e2cb04a74f85"))
