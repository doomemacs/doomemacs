;; -*- no-byte-compile: t; -*-
;;; completion/company/packages.el

(package! company :pin "ee3177cdad47cbe92242eeb52c7bdb9505282db6")
(package! company-dict :pin "cd7b8394f6014c57897f65d335d6b2bd65dab1f4")
(when (featurep! +childframe)
  (package! company-box :pin "aa5f09a5492344e3cc831f0f169a6a8345dec358"))
