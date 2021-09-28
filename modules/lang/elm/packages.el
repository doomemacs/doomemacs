;; -*- no-byte-compile: t; -*-
;;; lang/elm/packages.el

(package! elm-mode :pin "f2e2d0053f3272d9fc0c2e16c8d17d97724cf524")
(when (featurep! :checkers syntax)
  (package! flycheck-elm :pin "1b60050efd4729bfba548f3e5adbcb58436667cb"))
