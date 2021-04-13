;; -*- no-byte-compile: t; -*-
;;; lang/scheme/packages.el

(package! geiser
  :recipe (:host gitlab :repo "emacs-geiser/geiser")
  :pin "aa26163aa81b5af3bc5bbf23bec8b5776de3a8bc")

(when (featurep! :checkers syntax)
  (package! flycheck-guile
    :recipe (:host github :repo "flatwhatson/flycheck-guile")
    :pin "e46d6e5453dd7471309fae6549445c48e6d8f340"))
