;; -*- no-byte-compile: t; -*-
;;; lang/elm/packages.el

(package! elm-mode :pin "5797500d74dd8544a89938c09d3cd5cdf7b5d7bc")
(when (modulep! :checkers syntax)
  (package! flycheck-elm :pin "1b60050efd4729bfba548f3e5adbcb58436667cb"))
