;; -*- no-byte-compile: t; -*-
;;; completion/company/packages.el

(package! company :pin "d5053561cb")
(package! company-dict :pin "cd7b8394f6")
(package! company-prescient :pin "7fd8c3b802")
(when (featurep! +childframe)
  (package! company-box :pin "8fc6168f2d"))
