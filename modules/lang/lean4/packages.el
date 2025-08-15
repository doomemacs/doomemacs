;; -*- no-byte-compile: t; -*-
;;; lang/lean4/packages.el

(package! lean4-mode :pin "1388f9d1429e38a39ab913c6daae55f6ce799479"
	:recipe (:host github
		:repo "leanprover/lean4-mode"
		:files ("*.el" "data")))
