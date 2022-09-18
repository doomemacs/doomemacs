;; -*- no-byte-compile: t; -*-
;;; lang/rest/packages.el

(package! restclient :pin "9e2cfa86529133eba6c9ef53794be182f15e4c21")
(when (modulep! :completion company)
  (package! company-restclient :pin "e5a3ec54edb44776738c13e13e34c85b3085277b"))

(when (modulep! +jq)
  (package! jq-mode :pin "071c1c29bac30351ad338136f2b625e5601365cd")
  (package! restclient-jq :pin "ae79e7dd283890072da69b8f48aeec1afd0d9442"))
