;; -*- no-byte-compile: t; -*-
;;; checkers/grammar/packages.el

(when (and (modulep! +lsp)
           (not (modulep! :tools lsp +eglot)))
  (package! lsp-ltex :pin "18b0e8608408f9e913d89075e78c2b4e3f69cf1c"))
(unless (modulep! +lsp)
  (package! langtool :pin "8276eccc5587bc12fd205ee58a7a982f0a136e41"))
(package! writegood-mode :pin "ed42d918d98826ad88928b7af9f2597502afc6b0")
