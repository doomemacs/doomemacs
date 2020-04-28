;; -*- no-byte-compile: t; -*-
;;; lang/elm/packages.el

(package! elm-mode :pin "7782be0814")
(when (featurep! :checkers syntax)
  (package! flycheck-elm :pin "1b60050efd"))
