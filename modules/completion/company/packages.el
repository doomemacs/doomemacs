;; -*- no-byte-compile: t; -*-
;;; completion/company/packages.el

(package! company :pin "2ca3e29abf87392714bc2b26e50e1c0f4b9f4e2c")
(package! company-dict :pin "cd7b8394f6014c57897f65d335d6b2bd65dab1f4")
(when (modulep! +childframe)
  (package! company-box :pin "766546b2668b5ef4eb4abbde632c9acd370c7788"))
