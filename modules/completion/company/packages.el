;; -*- no-byte-compile: t; -*-
;;; completion/company/packages.el

(package! company :pin "5eb7d868c2a13bbfb14060b79d69f0d59f6a5f60")
(package! company-dict :pin "cd7b8394f6014c57897f65d335d6b2bd65dab1f4")
(when (featurep! +childframe)
  (package! company-box :pin "889d723786df2de27d248c9965149d04af44273a"))
