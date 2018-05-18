;; -*- no-byte-compile: t; -*-
;;; completion/company/packages.el

(package! company)
(package! company-dict)
(package! company-statistics)
(when (and EMACS26+ (featurep! +childframe))
  (package! company-box))
