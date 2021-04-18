;; -*- no-byte-compile: t; -*-
;;; completion/company/packages.el

(package! company :pin "7207cb143829edbcaa2a4b4c0011090747d2207c")
(package! company-dict :pin "cd7b8394f6014c57897f65d335d6b2bd65dab1f4")
(when (featurep! +childframe)
  (package! company-box :pin "ec8f44674dc10dd4d50785a1f97820b29d392ea2"))
