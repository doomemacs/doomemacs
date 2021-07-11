;; -*- no-byte-compile: t; -*-
;;; lang/raku/packages.el

(package! raku-mode :pin "7496ad3a03bed613c259405ec8839ae02950fdb1")

(when (featurep! :checkers syntax)
  (package! flycheck-raku
    :recipe (:host github :repo "widefox/flycheck-raku")
    :pin "b1acccd6e9d9753022571ee96d18b8e9f3227c65"))
