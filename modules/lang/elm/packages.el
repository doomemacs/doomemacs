;; -*- no-byte-compile: t; -*-
;;; lang/elm/packages.el

(package! elm-mode :pin "dd868e55ff")
(when (featurep! :checkers syntax)
  (package! flycheck-elm :pin "debd0af563"))
