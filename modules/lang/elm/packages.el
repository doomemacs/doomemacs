;; -*- no-byte-compile: t; -*-
;;; lang/elm/packages.el

(package! elm-mode :pin "699841865e1bd5b7f2077baa7121510b6bcad3c7")
(when (modulep! :checkers syntax -flymake)
  (package! flycheck-elm :pin "1b60050efd4729bfba548f3e5adbcb58436667cb"))
