;; -*- no-byte-compile: t; -*-
;;; lang/rest/packages.el

(package! restclient :pin "2cc1fd3496f57288de3f97c27a5f018284db2d23")
(when (featurep! :completion company)
  (package! company-restclient :pin "e5a3ec54edb44776738c13e13e34c85b3085277b"))
