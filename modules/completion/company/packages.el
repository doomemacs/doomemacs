;; -*- no-byte-compile: t; -*-
;;; completion/company/packages.el

(package! company :pin "41f07c7d401c1374a76f3004a3448d3d36bdf347")
(package! company-dict :pin "cd7b8394f6014c57897f65d335d6b2bd65dab1f4")
(when (modulep! +childframe)
  (package! company-box :pin "c4f2e243fba03c11e46b1600b124e036f2be7691"))
