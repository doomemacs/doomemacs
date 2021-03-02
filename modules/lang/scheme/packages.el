;; -*- no-byte-compile: t; -*-
;;; lang/scheme/packages.el

(package! geiser :pin "26dd2f4ae0f44879b5273bf87cdd42b8ec4140a1")

(when (featurep! :checkers syntax)
  (package! flycheck-guile
    :recipe (:host github :repo "flatwhatson/flycheck-guile")
    :pin "e46d6e5453dd7471309fae6549445c48e6d8f340"))
