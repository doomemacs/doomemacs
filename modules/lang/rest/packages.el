;; -*- no-byte-compile: t; -*-
;;; lang/rest/packages.el

(package! restclient :pin "426507f8f7029347fbbc78ce3b963df718abc3ce")
(when (modulep! :completion company)
  (package! company-restclient :pin "e5a3ec54edb44776738c13e13e34c85b3085277b"))

(when (modulep! +jq)
  (package! jq-mode :pin "39acc77a63555b8556b8163be3d9b142d173c795")
  (package! restclient-jq :pin "426507f8f7029347fbbc78ce3b963df718abc3ce"))
