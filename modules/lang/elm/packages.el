;; -*- no-byte-compile: t; -*-
;;; lang/elm/packages.el

(package! elm-mode)
(when (featurep! :tools flycheck)
  (package! flycheck-elm))

