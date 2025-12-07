;; -*- no-byte-compile: t; -*-
;;; completion/company/packages.el

(package! company :pin "4ff89f7369227fbb89fe721d1db707f1af74cd0f")
(package! company-dict :pin "cd7b8394f6014c57897f65d335d6b2bd65dab1f4")
(when (modulep! +childframe)
  (package! company-box :pin "c4f2e243fba03c11e46b1600b124e036f2be7691"))
