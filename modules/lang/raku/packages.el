;; -*- no-byte-compile: t; -*-
;;; lang/raku/packages.el

(package! raku-mode :pin "e0639c89a3a29e9196e298951da6c3a79fb944e8")

(when (featurep! :checkers syntax)
  (package! flycheck-raku
    :recipe (:host github :repo "widefox/flycheck-raku")
    :pin "046f35abe0c61967157e151126e4dd7ec5d1c004"))
