;; -*- no-byte-compile: t; -*-
;;; lang/julia/packages.el

(package! julia-mode :pin "7a8c868e0d3e51ba4a2c621ee22ca9599e0e4bbb")
(package! julia-repl :pin "4947319bc948b3f80d61b0d65a719737275949b8")

(when (modulep! +lsp)
  (if (modulep! :tools lsp +eglot)
      (package! eglot-jl :pin "7dc604fe42a459a987853d065cd6d0f3c4cbc02a")
    (package! lsp-julia :pin "c869b2f6c05a97e5495ed3cc6710a33b4faf41a2")))

(when (modulep! +snail)
  (package! julia-snail :pin "d36653bb938050cfabbe3c1ea6d4575071085577"))
