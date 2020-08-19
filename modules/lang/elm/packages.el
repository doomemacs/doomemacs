;; -*- no-byte-compile: t; -*-
;;; lang/elm/packages.el

(package! elm-mode :pin "363da4b47c9de1ff091a8caf95fccc34188d59a3")
(when (featurep! :checkers syntax)
  (package! flycheck-elm :pin "1b60050efd4729bfba548f3e5adbcb58436667cb"))
