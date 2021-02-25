;; -*- no-byte-compile: t; -*-
;;; completion/company/packages.el

(package! company :pin "88001d794d963049339883216b6606de0a1209ea")
(package! company-dict :pin "cd7b8394f6014c57897f65d335d6b2bd65dab1f4")
(when (featurep! +childframe)
  (package! company-box :pin "ec8f44674dc10dd4d50785a1f97820b29d392ea2"))
