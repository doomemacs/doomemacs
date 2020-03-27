;; -*- no-byte-compile: t; -*-
;;; completion/company/packages.el

(package! company :pin "61ddd9afb5")
(package! company-dict :pin "cd7b8394f6")
(package! company-prescient :pin "a194852e80")
(when (featurep! +childframe)
  (package! company-box :pin "8fc6168f2d"))
