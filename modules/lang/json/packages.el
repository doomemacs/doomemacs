;; -*- no-byte-compile: t; -*-
;;; lang/json/packages.el

(package! json-mode :pin "0e819e519a")
(package! json-snatcher :pin "c4cecc0a50")
(when (featurep! :completion ivy)
  (package! counsel-jq :pin "b14dfc5c18"))
