;; -*- no-byte-compile: t; -*-
;;; completion/company/packages.el

(package! company :pin "b0a522ac5bf8ba3d2f4f22e3aa846a4f82978a16")
(package! company-dict :pin "cd7b8394f6014c57897f65d335d6b2bd65dab1f4")
(when (modulep! +childframe)
  (package! company-box :pin "c4f2e243fba03c11e46b1600b124e036f2be7691"))
