;; -*- no-byte-compile: t; -*-
;;; lang/elm/packages.el

(package! elm-mode)
(when (featurep! :feature syntax-checker)
  (package! flycheck-elm))

