;; -*- no-byte-compile: t; -*-
;;; completion/company/packages.el

(package! company :pin "d77184094b9a45b204813d824918e1ec2aac8504")
(package! company-dict :pin "cd7b8394f6014c57897f65d335d6b2bd65dab1f4")
(when (featurep! +childframe)
  (package! company-box :pin "c8a867163b15586cc9ed4accb992094308e54a9a"))
(when (featurep! +prescient)
  (package! company-prescient :pin "027c2137a8d9e01a1d4c7b5e5d98da017dd2d48e"))
