;; -*- no-byte-compile: t; -*-
;;; lang/rest/packages.el

(package! restclient :pin "bba663590f4f34e9b39cc30c9563ca8f829cf574")
(when (modulep! :completion company)
  (package! company-restclient :pin "e5a3ec54edb44776738c13e13e34c85b3085277b"))

(when (modulep! +jq)
  (package! jq-mode :pin "3275c3f53fdc60c8065a8b05395b559ecb9edfaf")
  (package! restclient-jq :pin "bba663590f4f34e9b39cc30c9563ca8f829cf574"))
