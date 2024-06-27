;; -*- no-byte-compile: t; -*-
;;; lang/common-lisp/packages.el

(when (package! sly :pin "ba40c8f054ec3b7040a6c36a1ef3e9596b936421")
  (package! sly-asdf :pin "6f9d751469bb82530db1673c22e7437ca6c95f45")
  (package! sly-quicklisp :pin "34c73d43dd9066262387c626c17a9b486db07b2d")
  (package! sly-stepper :recipe (:host github :repo "joaotavora/sly-stepper"
                                 :files (:defaults "*.lisp" "*.asd")))
  (package! sly-macrostep :pin "5113e4e926cd752b1d0bcc1508b3ebad5def5fad")
  (package! sly-repl-ansi-color :pin "b9cd52d1cf927bf7e08582d46ab0bcf1d4fb5048")
  (package! sly-overlay :pin "4c6135c26051fa4cc87d57d7d127aaae5bd40860"))
