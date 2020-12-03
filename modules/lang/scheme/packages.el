;; -*- no-byte-compile: t; -*-
;;; lang/scheme/packages.el

(package! geiser :pin "cd00be69b26e6fd748b183d127d8b6f4c91ba622")

(when (featurep! :checkers syntax)
  (package! flycheck-guile
    :recipe (:host github :repo "flatwhatson/flycheck-guile")
    :pin "e46d6e5453dd7471309fae6549445c48e6d8f340"))
