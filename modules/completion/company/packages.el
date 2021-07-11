;; -*- no-byte-compile: t; -*-
;;; completion/company/packages.el

(package! company :pin "d77184094b9a45b204813d824918e1ec2aac8504")
(package! company-dict :pin "cd7b8394f6014c57897f65d335d6b2bd65dab1f4")
(when (featurep! +childframe)
  (package! company-box :pin "c8a867163b15586cc9ed4accb992094308e54a9a"))
