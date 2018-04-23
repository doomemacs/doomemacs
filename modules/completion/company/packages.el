;; -*- no-byte-compile: t; -*-
;;; completion/company/packages.el

(package! company)
(package! company-dict)
(package! company-statistics)
(when (featurep! +childframe)
  (package! company-box))
