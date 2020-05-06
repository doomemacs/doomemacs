;; -*- no-byte-compile: t; -*-
;;; lang/perl/packages.el

(package! raku-mode :pin "d474216840251dc0efe4f4aa4f5c5f66ac26fa74")

(when (featurep! :checkers syntax)
  (package! flycheck-raku
    :recipe (:host github :repo "widefox/flycheck-raku")
    :pin "046f35abe0c61967157e151126e4dd7ec5d1c004"))
