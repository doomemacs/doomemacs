;; -*- no-byte-compile: t; -*-
;;; lang/elm/packages.el

(package! elm-mode)
(when (featurep! :checkers syntax)
  (package! flycheck-elm))

