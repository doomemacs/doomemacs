;; -*- no-byte-compile: t; -*-
;;; lang/elm/packages.el

(package! elm-mode :pin "90b72cd2c9bc4506f531bcdcd73fa2530d9f4f7c")
(when (modulep! :checkers syntax -flymake)
  (package! flycheck-elm :pin "1b60050efd4729bfba548f3e5adbcb58436667cb"))
