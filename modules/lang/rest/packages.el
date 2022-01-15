;; -*- no-byte-compile: t; -*-
;;; lang/rest/packages.el

(package! restclient :pin "176d9cb6552f04d98c33e29fc673862bdf3bca03")
(when (featurep! :completion company)
  (package! company-restclient :pin "e5a3ec54edb44776738c13e13e34c85b3085277b"))
