;; -*- no-byte-compile: t; -*-
;;; lang/common-lisp/packages.el

(when (package! sly :pin "ed17d2c2bd7aead0fbb09c3d22861c80a522a097")
  (package! sly-asdf :pin "6f9d751469bb82530db1673c22e7437ca6c95f45")
  (package! sly-quicklisp :pin "34c73d43dd9066262387c626c17a9b486db07b2d")
  (package! sly-stepper :recipe (:host github :repo "joaotavora/sly-stepper"
                                 :files (:defaults "*.lisp" "*.asd")))
  (package! sly-macrostep :pin "5113e4e926cd752b1d0bcc1508b3ebad5def5fad")
  (package! sly-repl-ansi-color :pin "b9cd52d1cf927bf7e08582d46ab0bcf1d4fb5048")
  (package! sly-overlay :pin "916b50297a1f3bb110f840b89b8717d194623e5f"))
