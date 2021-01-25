;; -*- no-byte-compile: t; -*-
;;; completion/company/packages.el

(package! company :pin "97cfbc3967c195fb4ccb171735b9b1dea97e681a")
(package! company-dict :pin "cd7b8394f6014c57897f65d335d6b2bd65dab1f4")
(when (featurep! +childframe)
  (package! company-box :pin "be37a9a30dc112ab172af21af694e2cb04a74f85"))
(when (featurep! +prescient)
  (package! company-prescient :pin "5d139e5b1fe03ccaddff8c250ab8e9d795071b95"))
