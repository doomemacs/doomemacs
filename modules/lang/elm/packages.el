;; -*- no-byte-compile: t; -*-
;;; lang/elm/packages.el

(package! elm-mode :pin "188b9c743d8ec99ff7735d2581999d07f43b5bbe")
(when (featurep! :checkers syntax)
  (package! flycheck-elm :pin "1b60050efd4729bfba548f3e5adbcb58436667cb"))
