;; -*- no-byte-compile: t; -*-
;;; completion/company/packages.el

(package! company :pin "5bb6f6d3d44ed919378e6968a06feed442165545")
(package! company-dict :pin "cd7b8394f6014c57897f65d335d6b2bd65dab1f4")
(when (modulep! +childframe)
  (package! company-box :pin "c4f2e243fba03c11e46b1600b124e036f2be7691"))
