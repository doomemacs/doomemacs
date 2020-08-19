;; -*- no-byte-compile: t; -*-
;;; completion/company/packages.el

(package! company :pin "54f60ef523878c4d332f29df380f36cf2f165935")
(package! company-dict :pin "cd7b8394f6014c57897f65d335d6b2bd65dab1f4")
(when (featurep! +childframe)
  (package! company-box :pin "20384f0e382c063173b9d863344b1b23bc1e4954"))
