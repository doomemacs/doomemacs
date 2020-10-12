;; -*- no-byte-compile: t; -*-
;;; lang/common-lisp/packages.el

(when (package! sly :pin "fce4f9be39b6a40a4128458ff34151eecb5e27de")
  (package! sly-macrostep :pin "5113e4e926cd752b1d0bcc1508b3ebad5def5fad")
  (package! sly-repl-ansi-color :pin "b9cd52d1cf927bf7e08582d46ab0bcf1d4fb5048"))
