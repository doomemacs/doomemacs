;; -*- no-byte-compile: t; -*-
;;; lang/rest/packages.el

(package! restclient :pin "e2a2b13482d72634f8e49864cd9e5c907a5fe137")
(when (modulep! :completion company)
  (package! company-restclient :pin "e5a3ec54edb44776738c13e13e34c85b3085277b"))

(when (modulep! +jq)
  (package! jq-mode :pin "37028e120041a489eff352b79202361cb232377e")
  (package! restclient-jq :pin "e2a2b13482d72634f8e49864cd9e5c907a5fe137"))
