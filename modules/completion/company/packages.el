;; -*- no-byte-compile: t; -*-
;;; completion/company/packages.el

(package! company)
(package! company-dict)
(when (and EMACS26+ (featurep! +childframe))
  (package! company-box))
