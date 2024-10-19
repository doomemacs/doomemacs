;; -*- no-byte-compile: t; -*-
;;; lang/raku/packages.el

(package! raku-mode :pin "14f9b9bba08c0bbb7f3895380d0b1a9feb7a168d")

(when (modulep! :checkers syntax -flymake)
  (package! flycheck-raku
    :recipe (:host github :repo "widefox/flycheck-raku")
    :pin "b1acccd6e9d9753022571ee96d18b8e9f3227c65"))
