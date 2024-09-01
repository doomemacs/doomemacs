;; -*- no-byte-compile: t; -*-
;;; completion/company/packages.el

(package! company :pin "e1d331a64ec39fe28c5be28cabf812e3eaab5b0f")
(package! company-dict :pin "cd7b8394f6014c57897f65d335d6b2bd65dab1f4")
(when (modulep! +childframe)
  (package! company-box :pin "c4f2e243fba03c11e46b1600b124e036f2be7691"))
