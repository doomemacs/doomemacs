;; -*- no-byte-compile: t; -*-
;;; lang/rest/packages.el

(package! restclient :pin "abc307b965bf6720bc466281f2e204cd5ce37dc3")
(when (featurep! :completion company)
  (package! company-restclient :pin "e5a3ec54edb44776738c13e13e34c85b3085277b"))
