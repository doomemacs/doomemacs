;; -*- no-byte-compile: t; -*-
;;; lang/json/packages.el

(package! json-mode :pin "0e819e519ae17a2686e0881c4ca51fa873fa9b83")
(package! json-snatcher :pin "c4cecc0a5051bd364373aa499c47a1bb7a5ac51c")
(when (featurep! :completion ivy)
  (package! counsel-jq :pin "b14dfc5c18d991c3b3051c3cbb244d5923b3a327"))
