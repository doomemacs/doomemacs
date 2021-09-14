;; -*- no-byte-compile: t; -*-
;;; lang/julia/packages.el

(package! julia-mode :pin "06f6fdb94cdd88db7bb40b8f511a386605711408")
(package! julia-repl :pin "3f888ecd30f613ed50f67c614be0b42b7546c693")

(when (featurep! +lsp)
  (if (featurep! :tools lsp +eglot)
      (package! eglot-jl :pin "49f170e01c5a107c2cb662c00544d827eaa2c4d8")
    (package! lsp-julia :pin "809da95c05fe668acbae5a35e03082d9b9577728")))
