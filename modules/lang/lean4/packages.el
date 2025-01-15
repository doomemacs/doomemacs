;; -*- no-byte-compile: t; -*-
;;; lang/lean4/packages.el

(package! lean4-mode :pin "d1c936409ade7d93e67107243cbc0aa55cda7fd5"
	:recipe (:host github
		:repo "leanprover/lean4-mode"
		:files ("*.el" "data")))
