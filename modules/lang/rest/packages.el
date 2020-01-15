;; -*- no-byte-compile: t; -*-
;;; lang/rest/packages.el

(package! restclient :pin "e8ca809ace13549a1ddffb4e4aaa5d5fce750f3d")
(when (featurep! :completion company)
  (package! company-restclient :pin "e5a3ec54edb44776738c13e13e34c85b3085277b"))
