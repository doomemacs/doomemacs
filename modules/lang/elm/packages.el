;; -*- no-byte-compile: t; -*-
;;; lang/elm/packages.el

(package! elm-mode :pin "5df694e307cf8fa5a3555d800984aa4ebb40664f")
(when (featurep! :checkers syntax)
  (package! flycheck-elm :pin "debd0af563cb6c2944367a691c7fa3021d9378c1"))
