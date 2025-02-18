;; -*- no-byte-compile: t; -*-
;;; lang/lean4/packages.el

(package! lean4-mode :pin "76895d8939111654a472cfc617cfd43fbf5f1eb6"
	:recipe (:host github
		:repo "leanprover/lean4-mode"
		:files ("*.el" "data")))
