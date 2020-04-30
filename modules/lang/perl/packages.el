;; -*- no-byte-compile: t; -*-
;;; lang/perl/packages.el

(package! raku-mode :pin "44529c097f98723067f852c6496d91257978c1e2")

(when (featurep! :checkers syntax)
  (package! flycheck-raku
    :recipe (:host github :repo "widefox/flycheck-raku")
    :pin "046f35abe0c61967157e151126e4dd7ec5d1c004"))
