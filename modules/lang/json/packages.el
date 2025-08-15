;; -*- no-byte-compile: t; -*-
;;; lang/json/packages.el

(package! json-mode :pin "466d5b563721bbeffac3f610aefaac15a39d90a9")
(package! json-snatcher :pin "b28d1c0670636da6db508d03872d96ffddbc10f2")
(when (modulep! :completion ivy)
  (package! counsel-jq :pin "8cadd2e96470402ede4881b4e955872976443689"))
