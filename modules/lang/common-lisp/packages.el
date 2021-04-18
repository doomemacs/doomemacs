;; -*- no-byte-compile: t; -*-
;;; lang/common-lisp/packages.el

(when (package! sly :pin "5966d68727898fa6130fb6bb02208f70aa8d5ce3")
  (package! sly-macrostep :pin "5113e4e926cd752b1d0bcc1508b3ebad5def5fad")
  (package! sly-repl-ansi-color :pin "b9cd52d1cf927bf7e08582d46ab0bcf1d4fb5048"))
