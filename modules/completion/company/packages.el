;; -*- no-byte-compile: t; -*-
;;; completion/company/packages.el

(package! company :pin "4462e7d399c3d2e1ea1f5d018f4537f1f3d5acfc")
(package! company-dict :pin "cd7b8394f6014c57897f65d335d6b2bd65dab1f4")
(when (featurep! +childframe)
  (package! company-box :pin "d8f71fde7a3301a62f9f6f627aa51610a294f4df"))
