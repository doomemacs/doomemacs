;; -*- no-byte-compile: t; -*-
;;; checkers/grammar/packages.el

(when (and (modulep! +lsp)
           (not (modulep! :tools lsp +eglot)))
  (package! lsp-ltex :pin "18b0e8608408f9e913d89075e78c2b4e3f69cf1c")
  ;; Optional dependency of lsp-ltex, needed for installing/updating ltex-ls LSP server
  (package! github-tags
    :recipe (:host github
             :repo "jcs-elpa/github-tags")
    :pin "7b02d6e883ac3d766106de30c60b22609c4515f9"))
(unless (modulep! +lsp)
  (package! langtool :pin "8276eccc5587bc12fd205ee58a7a982f0a136e41"))
(package! writegood-mode :pin "ed42d918d98826ad88928b7af9f2597502afc6b0")
