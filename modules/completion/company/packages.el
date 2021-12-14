;; -*- no-byte-compile: t; -*-
;;; completion/company/packages.el

(package! company :pin "8b58e5895c2eaf8686de0e25c807b00fdb205c7a")
(package! company-dict :pin "cd7b8394f6014c57897f65d335d6b2bd65dab1f4")
(when (featurep! +childframe)
  (package! company-box :pin "f9cbbc7df8efbb56a8d31a5b422d158660d9109e"))
