;; -*- no-byte-compile: t; -*-
;;; lang/common-lisp/packages.el

(when (package! sly :pin "41f4d650485217aa1f2afa7c159418f103a09231")
  (package! sly-macrostep :pin "5113e4e926cd752b1d0bcc1508b3ebad5def5fad")
  (package! sly-repl-ansi-color :pin "b9cd52d1cf927bf7e08582d46ab0bcf1d4fb5048"))
