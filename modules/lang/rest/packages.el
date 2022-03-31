;; -*- no-byte-compile: t; -*-
;;; lang/rest/packages.el

(package! restclient :pin "9e2cfa86529133eba6c9ef53794be182f15e4c21")
(when (featurep! :completion company)
  (package! company-restclient :pin "e5a3ec54edb44776738c13e13e34c85b3085277b"))
