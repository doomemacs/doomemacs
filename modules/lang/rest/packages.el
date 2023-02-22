;; -*- no-byte-compile: t; -*-
;;; lang/rest/packages.el

(package! restclient :pin "0ba72816f92f3d5906cdf76f418fd0a3ee72809b")
(when (modulep! :completion company)
  (package! company-restclient :pin "e5a3ec54edb44776738c13e13e34c85b3085277b"))

(when (modulep! +jq)
  (package! jq-mode :pin "071c1c29bac30351ad338136f2b625e5601365cd")
  (package! restclient-jq :pin "0ba72816f92f3d5906cdf76f418fd0a3ee72809b"))
