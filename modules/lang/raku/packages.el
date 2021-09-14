;; -*- no-byte-compile: t; -*-
;;; lang/raku/packages.el

(package! raku-mode :pin "eaac071f1779b6545d6593f6a05f09b4e58c98db")

(when (featurep! :checkers syntax)
  (package! flycheck-raku
    :recipe (:host github :repo "widefox/flycheck-raku")
    :pin "b1acccd6e9d9753022571ee96d18b8e9f3227c65"))
