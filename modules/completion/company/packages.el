;; -*- no-byte-compile: t; -*-
;;; completion/company/packages.el

(package! company :pin "1887974e7ad50cb4884fbec27985078bba754b60")
(package! company-dict :pin "cd7b8394f6014c57897f65d335d6b2bd65dab1f4")
(when (featurep! +childframe)
  (package! company-box :pin "156f65cfbf690ed84e0e84f90277d665d873ff24"))
