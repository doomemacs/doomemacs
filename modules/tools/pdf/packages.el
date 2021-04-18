;; -*- no-byte-compile: t; -*-
;;; tools/pdf/packages.el

(package! pdf-tools
  :recipe (:host github :repo "vedang/pdf-tools") ; active fork
  :pin "35e12b0813e86c2e15793e75a1f8649fe7ab909e")

(package! saveplace-pdf-view :pin "54ed966b842501c3c092dbf57b372e37b033c578")
