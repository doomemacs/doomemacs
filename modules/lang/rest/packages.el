;; -*- no-byte-compile: t; -*-
;;; lang/rest/packages.el

(package! restclient)
(when (featurep! :completion company)
  (package! company-restclient))

