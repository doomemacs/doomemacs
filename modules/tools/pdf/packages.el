;; -*- no-byte-compile: t; -*-
;;; tools/pdf/packages.el

(package! pdf-tools
  :recipe (:host github
           :repo "vedang/pdf-tools")
  :pin "d262cf9e19d57c6567e06e51d109150c20753839")

(package! saveplace-pdf-view :pin "54ed966b842501c3c092dbf57b372e37b033c578")
