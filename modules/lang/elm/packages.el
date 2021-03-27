;; -*- no-byte-compile: t; -*-
;;; lang/elm/packages.el

(package! elm-mode :pin "e9fcf9cc2779cf7f5ae7ee4be339164b26755c69")
(when (featurep! :checkers syntax)
  (package! flycheck-elm :pin "1b60050efd4729bfba548f3e5adbcb58436667cb"))
