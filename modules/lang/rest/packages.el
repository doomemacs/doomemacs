;; -*- no-byte-compile: t; -*-
;;; lang/rest/packages.el

(package! restclient :pin "ac8aad6c6b9e9d918062fa3c89c22c2f4ec48bc3")
(when (featurep! :completion company)
  (package! company-restclient :pin "e5a3ec54edb44776738c13e13e34c85b3085277b"))
