;; -*- no-byte-compile: t; -*-
;;; lang/common-lisp/packages.el

(when (package! sly :pin "3278819ddf71d16444e6cea293dd41ca83ea9bae")
  (package! sly-macrostep :pin "5113e4e926cd752b1d0bcc1508b3ebad5def5fad")
  (package! sly-repl-ansi-color :pin "b9cd52d1cf927bf7e08582d46ab0bcf1d4fb5048"))
