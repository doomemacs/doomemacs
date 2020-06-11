;; -*- no-byte-compile: t; -*-
;;; lang/scheme/packages.el

(package! geiser :pin "0c86289d7b2af07e3653364219e00103c8540edf")

(when (featurep! :checkers syntax)
  (package! flycheck-guile
    :recipe (:host github :repo "flatwhatson/flycheck-guile")
    :pin "f37b6143776d15b3e7907e7621f6f96f8b1aec48"))
