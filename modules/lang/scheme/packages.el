;; -*- no-byte-compile: t; -*-
;;; lang/scheme/packages.el

(package! geiser :pin "8e61c27b628373523b7c467d5f71aac8c873258b")

(when (featurep! :checkers syntax)
  (package! flycheck-guile
    :recipe (:host github :repo "flatwhatson/flycheck-guile")
    :pin "e46d6e5453dd7471309fae6549445c48e6d8f340"))
