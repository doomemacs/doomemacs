;; -*- no-byte-compile: t; -*-
;;; lang/json/packages.el

(package! json-mode :pin "bfd1557aaa20b7518b808fdc869f094b52205234")
(package! json-snatcher :pin "b28d1c0670636da6db508d03872d96ffddbc10f2")
(when (modulep! :completion ivy)
  (package! counsel-jq :pin "8cadd2e96470402ede4881b4e955872976443689"))
