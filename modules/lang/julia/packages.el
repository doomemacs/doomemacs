;; -*- no-byte-compile: t; -*-
;;; lang/julia/packages.el

(package! julia-mode :pin "d360ad5285b8a0be1818fd6c2b4307c34e468c6e")
(package! julia-repl :pin "801d0fc3d8f6f08f66a11515e6517739a0b312a1")

(when (modulep! +lsp)
  (if (modulep! :tools lsp +eglot)
      (package! eglot-jl :pin "1d9cab682380f37ca1e9e9933cda13164600706d")
    (package! lsp-julia :pin "c869b2f6c05a97e5495ed3cc6710a33b4faf41a2")))

(when (modulep! +snail)
  (package! julia-snail :pin "dff92c4250e40a6cc106f0ea993f9631ad55eb7c"))
