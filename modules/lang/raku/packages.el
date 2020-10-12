;; -*- no-byte-compile: t; -*-
;;; lang/raku/packages.el

(package! raku-mode :pin "8a6e17f1749c084251d19c3d58b9c1495891db6d")

(when (featurep! :checkers syntax)
  (package! flycheck-raku
    :recipe (:host github :repo "widefox/flycheck-raku")
    :pin "046f35abe0c61967157e151126e4dd7ec5d1c004"))
