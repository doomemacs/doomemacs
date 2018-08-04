;; -*- no-byte-compile: t; -*-
;;; completion/company/packages.el

(package! company)
(package! company-dict)
(package! company-prescient)
(when (and EMACS26+ (featurep! +childframe))
  (package! company-box))
