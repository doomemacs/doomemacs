;; -*- no-byte-compile: t; -*-
;;; lang/julia/packages.el

(package! julia-mode :pin "2dfc869ff6b3878407fe7226669dfaae8d38d541")
(package! julia-repl :pin "4947319bc948b3f80d61b0d65a719737275949b8")

(when (modulep! +lsp)
  (if (modulep! :tools lsp +eglot)
      (package! eglot-jl :pin "1d9cab682380f37ca1e9e9933cda13164600706d")
    (package! lsp-julia :pin "c869b2f6c05a97e5495ed3cc6710a33b4faf41a2")))

(when (modulep! +snail)
  (package! julia-snail :pin "a25ce847480a0c2bed24fad3f1ee62904c9c93a5"))
