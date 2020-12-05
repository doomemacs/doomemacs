;; -*- no-byte-compile: t; -*-
;;; lang/json/packages.el

(package! json-mode :pin "0e819e519ae17a2686e0881c4ca51fa873fa9b83")
(package! json-snatcher :pin "b28d1c0670636da6db508d03872d96ffddbc10f2")
(when (featurep! :completion ivy)
  (package! counsel-jq :pin "f5bfed87a41adefebda020c31a4274df4b6550f5"))
