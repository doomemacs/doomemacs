;; -*- no-byte-compile: t; -*-
;;; completion/company/packages.el

(package! company :pin "d5053561cb166e03051c60e5c1e05a8a926dfee5")
(package! company-dict :pin "cd7b8394f6014c57897f65d335d6b2bd65dab1f4")
(package! company-prescient :pin "7fd8c3b8028da4733434940c4aac1209281bef58")
(when (featurep! +childframe)
  (package! company-box :pin "8fc6168f2d3a0275156dd3fdf46ba496adbab226"))
