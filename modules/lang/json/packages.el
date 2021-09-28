;; -*- no-byte-compile: t; -*-
;;; lang/json/packages.el

(package! json-mode :pin "0e819e519ae17a2686e0881c4ca51fa873fa9b83")
(package! json-snatcher :pin "b28d1c0670636da6db508d03872d96ffddbc10f2")
(when (featurep! :completion ivy)
  (package! counsel-jq :pin "8cadd2e96470402ede4881b4e955872976443689"))
