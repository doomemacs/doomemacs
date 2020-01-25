;; -*- no-byte-compile: t; -*-
;;; lang/rest/packages.el

(package! restclient :pin "e8ca809ace")
(when (featurep! :completion company)
  (package! company-restclient :pin "e5a3ec54ed"))
